/************************************************** -*- tab-width:4 -*- *
* CMake mode for Epsilon                   								*
* Written by Trent Lillehaugen											*
* tllilleh+epsilon@gmail.com											*
* Please email any comments or corrections.								*
* Last Modified: 2009/05/15												*
*************************************************************************
* User Variables:
*****************
* - cmake_indent: default (4)
*	indent size
* - cmake_auto_fill_close_expressions: default (1)
*	If set to 1, else endif, endwhile, endmacro, etc., command
*	expressions are automatically filled with the expression of the
*	matching begin command
*
********************
* Known Limitations:
********************
* - Multi-line strings can cause both coloring and auto-indention
* problems.  It is non-trivial to find the bounds of a multi-line
* string without parsing from the beginning of the buffer.  Note: CMake
* does not require a line continuation marker at the end of the
* previous line.  Perhaps we can search for the beginning of the
* command and start parsing from there for the beginning and end of
* strings.
************************************************************************/

#include "eel.h"
#include "colcode.h"

keytable cmake_tab;
user char cmake_indent = 4;
user int cmake_auto_fill_close_expressions = 1;

/* Color this range of the buffer. */
color_cmake_range(from, to) {
	int t = -1, s;

	if (from >= to) {
		return to;
	}

	save_var point, matchstart, matchend, case_fold = 1;

	point = from;
	to_begin_line();

	set_character_color(point, to, -1);


	for (; point < to; ) {

		if (!re_search(1, "[#\"%(%)]|%${|[0-9]+([%.0-9]+)?|@?[a-zA-Z][-0-9a-zA-Z_]*|[a-zA-Z][0-9a-zA-Z_]*[ \t]*%("))
			break;

		t = matchstart;
		switch (character(matchstart)) {
			case '#': // Comment.
				set_character_color(matchstart, give_end_line() + 1, color_class c_comment);
				nl_forward();
				break;

			case '"': // String literal
				point = t;
				re_search(1, "\"([^\"\\\n]|\\(.|\n))*[\"\n]");
				set_character_color(t, point, color_class c_string);
				s = point;
				point = t;
				while (re_search(1, "%${")) {
					if (character(matchstart - 1) == '\\') {
						/* The $ is esccaped.. This is not really a
						 * variable derefference. */
						continue;
					} else if (matchstart > s) {
						/* we are out of the string */
						break;
					}
					find_end_of_variable_dereference();
					set_character_color(matchstart, point, color_class c_special);
				}
				point = s;
				break;

			case '0': // Numerical literal can include version numbers such as 1.2.3
			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
			case '8':
			case '9':
				set_character_color(matchstart, point, color_class c_number);
				break;

			case '(': // Parens
			case ')':
				set_character_color(matchstart, point, color_class c_punctuation);
				break;

			case '$': // Variable Dereference
				find_end_of_variable_dereference();
				set_character_color(matchstart, point, color_class c_special);
				break;

			default: // Word, could be a command, variable, etc...
				if (character(point - 1) == '(') {
					/* This is a command. We could color-code build it
					 * commands differently by calling
					 * is_cmake_command() but that may confuse the
					 * user.  They will get errors if the use a
					 * function that does not exist - we don't need to
					 * color code for that */
					point = point - 1;
					set_character_color(matchstart, point, color_class c_function);
				} else {
					color_cmake_word(matchstart, point);
				}
				break;
		}
	}

	/* undo coloring not part of the from -> to region */
	if (to < t) {
		set_character_color(to, t, -1);
	}

	return point;
}

/* sets point to the far end of the variable dereference.  Allows for
 * nesting of variables such as: ${ABC_${DEF}} */
find_end_of_variable_dereference() {
	save_var matchstart;
	int open_var_count = 1;
	while(re_search(1, "%${|}|\n|#")) {
		char ch = character(matchstart);
		if (ch == '$') {
			open_var_count = open_var_count + 1;
		} else if (ch == '}') {
			open_var_count = open_var_count - 1;
			if (open_var_count == 0) {
				break;
			}
		} else if (ch == '\n') {
			/* Variables can not span multiple lines */
			point = matchstart;
			break;
		} else if (ch == '#') {
			/* Variables can not contain comments */
			point = matchstart;
			break;
		}
	}
}

/* Return 1 if point is contained in the middle of a string, 0
 * otherwise.  Currently this code only supports single line strings -
 * NOT mult-line strings. */
in_string() {
	save_var point;
	int p = point;
	int string_start;
	int string_end;
	to_begin_line();
	while (search(1, "\"")) {
		// start of string
		if (character(matchstart - 1) == '\\') {
			continue;
		}
		string_start = matchstart;
		if (p < string_start) {
			return 0;
		}

		while (search(1, "\"")) {
			if (character(matchstart - 1) == '\\') {
				continue;
			}
			// end of string.
			string_end = matchend - 1;
			if (p > string_start && p < string_end) {
				return 1;
			} else {
				break;
			}
		}
	}
	return 0;
}

/* Indent the current line */
do_cmake_indent() {
	int indent;
	save_var point;

	/* Find the previous non-comment line */
	do {
		if (!nl_reverse())
			return 0;
		to_begin_line();
	} while (parse_string(1, "[ \t]*(#|\n)"));
	to_indentation();
	indent = current_column();

	/* Check if the command on the previous line should increment indention */
	if (parse_string(1, "(else|(else)?if|macro|while|function|foreach)[ \t]*%(")) {
		indent = indent + cmake_indent;
	}

	/* If the previous line was an incomplete command increment
	 * idention */
	if (parse_string(1, "[a-zA-Z][0-9a-zA-Z_]*[ \t]*%([^%)^\n]*$")) {
		indent = indent + cmake_indent;
	}

	/* if the previous line closed an incomplete command decrement
	 * idention */

	int len = parse_string(1, ".+%)");
	if (len) {
		int p = point + len - 1;
		/* make sure any open parens we find are in strings */
		int found_open_paren = 0;
		while (search(1, "(")) {
			point = matchstart;
			if (point > p) {
				break;
			}
			if (!in_string()) {
				found_open_paren = 1;
				break;
			}
			point = matchend;
		}
		point = p;
		if (!found_open_paren && !in_string()) {
			indent = indent - cmake_indent;
		}
	}

	restore_vars();
	save_spot point;

	to_indentation();

	/* If the current line is an else, elseif, endif, or endmacro decrease the
	 * indentation */
	if (parse_string(1, "((else(if)?[ \t]*)|endif|endfunction|endwhile|endforeach|endmacro)[ \t]*%(")) {
		indent = indent - cmake_indent;
	}

	/* If the current line closes an incomplete command, decrease the
	 * indentation */
	if (parse_string(1, "%)")) {
		indent = indent - cmake_indent;
	}

	restore_vars();
	to_column(indent);
}

/* Automatically fill the expressions of closing commands */
cmake_fill_close_expressions() {
	char expr[1000];
	char open_command[15];
	char close_command[15];
	char re_open_command[50];
	char re_close_command[50];
	save_spot point;
	int found = 0;
	int first_arg_only = 0;

	/* If the current line is an elseif or endif find the
	 * matching if */
	to_indentation();
	if (parse_string(1, "(else|endif)[ \t]*%(")) {
		strcpy(open_command, "if");
		strcpy(close_command, "endif");
	} else if (parse_string(1, "endforeach[ \t]*%(")) {
		strcpy(open_command, "foreach");
		strcpy(close_command, "endforeach");
		first_arg_only = 1;
	} else if (parse_string(1, "endfunction[ \t]*%(")) {
		strcpy(open_command, "function");
		strcpy(close_command, "endfunction");
		first_arg_only = 1;
	} else if (parse_string(1, "endmacro[ \t]*%(")) {
		strcpy(open_command, "macro");
		strcpy(close_command, "endmacro");
		first_arg_only = 1;
	} else if (parse_string(1, "endwhile[ \t]*%(")) {
		strcpy(open_command, "while");
		strcpy(close_command, "endwhile");
	} else {
		return 0;
	}

	sprintf(re_open_command, "[ \t]*%s[ \t]*%(.*%)", open_command);
	sprintf(re_close_command, "[ \t]*%s[ \t]*%(.*%)", close_command);
	int level = 1;
	do {
		if (!nl_reverse()) {
			return 0;
		}
		to_begin_line();
		if (parse_string(1, re_open_command)) {
			level = level - 1;
			if (level == 0) {
				found = 1;
			}
		}

		if (parse_string(1, re_close_command)) {
			level = level + 1;
		}
	} while (!found);

	if (first_arg_only) {
		re_search(1, "%([ \t]*[^ \t%)]*[ \t%)]");
	} else {
		re_search(1, "%(.*%)");
	}
	int expr_start = matchstart + 1;
	int expr_end = point - 1;
	if (expr_end > expr_start) {
		grab(expr_start, expr_end, expr);
	}
	restore_vars();
	stuff(expr);
}

cmake_indent_close_paren() on cmake_tab[')'] {
	normal_character();

	save_spot point;
	to_indentation();
	do_cmake_indent();
	restore_vars();
}

cmake_indent_open_paren() on cmake_tab['('] {
	normal_character();

	save_spot point;
	to_indentation();
	do_cmake_indent();
	restore_vars();

	if (cmake_auto_fill_close_expressions) {
		cmake_fill_close_expressions();
	}
}

cmake_indenter() on cmake_tab['\t'] {
	int orig = point;
	int orig_column = current_column();

	if (maybe_indent_rigidly(0)) {
		return;
	}

	to_indentation();
	if (orig_column > current_column()) { /* if not in indentation */
		point = orig;
		to_column(orig_column + cmake_indent - (orig_column % cmake_indent));
	} else if (prev_cmd == C_INDENT) {	/* repeated, make bigger */
		to_column(orig_column + cmake_indent);
	} else {
		do_cmake_indent();
		if (!get_indentation(point) && !orig_column && this_cmd != CMD_INDENT_REG) { // 0 indent=>0 indent,
			to_column(cmake_indent);												 // but indent anyway
		}
	}
	if (this_cmd != CMD_INDENT_REG) {
		this_cmd = C_INDENT;
	}
}

command cmake_mode() {
	mode_default_settings();
	mode_keys = cmake_tab;		/* Use these keys. */
	major_mode = "CMake";
	strcpy(comment_start, "#[ \t]*");
	strcpy(comment_pattern, "#.*$");
	strcpy(comment_begin, "# ");
	strcpy(comment_end, "");
	buffer_maybe_break_line = generic_maybe_break_line;
	fill_mode = (misc_language_fill_mode & 8) != 0;
	recolor_range = color_cmake_range;	// set up coloring rules
	indenter = do_cmake_indent;
	recolor_from_here = recolor_from_top;
	auto_indent = 1;
	if (want_code_coloring)		// maybe turn on coloring
		when_setting_want_code_coloring();
	try_calling("cmake-mode-hook");
	drop_all_colored_regions();
	make_mode();
}

when_loading() {

}

suffix_txt() {
	if (!strfcmp(get_tail(filename), "cmakelists.txt")) {
		cmake_mode();
	}
}

suffix_in() {
	/* remove the .in extention and try color coding on the next
	 * extenion of the file..  For example:
	 * config.h.in will try calling suff_h
	 * */
	char newfilename[FNAMELEN];
	char suffixcall[FNAMELEN + 7];

	int len;
	strcpy(newfilename, get_tail(filename));
	len = strlen(newfilename);
	newfilename[len - strlen(".in")] = 0;
	sprintf(suffixcall, "suffix_%s", get_extension(newfilename) + 1);
	try_calling(suffixcall);
}

suffix_cmake() {
	cmake_mode();
}

suffix_ctest() {
	cmake_mode();
}

color_cmake_word(int from, int to) {
	char buf[1000];

	if (to - from > sizeof(buf) / sizeof(char) - 10)
		to = from + sizeof(buf) / sizeof(char) - 10;

	buf[0] = '|';
	grab(from, to, buf + 1);
	strcpy(buf + (to - from) + 1, "|");

	if (is_cmake_variable(buf)) {
		set_character_color(from, to, color_class c_keyword);
	} else if (is_cmake_argument(buf)) {
		set_character_color(from, to, color_class c_punctuation);
	}
}

is_cmake_argument(char *p) {
	/* TODO: This list may not be complete.  I don't know if there is
	 * an easy way to get this list automatically generated. */

	/* This could be greatly improved by checking if the argument is
	 * valid for the current command.  However, that would be a lot of
	 * extra effort. */
	char *found = strstr("|@ONLY|ABSOLUTE|ABSTRACT|AFTER|ALL|AND|APPEND|ARCHIVE|ARGS|ASCII"
			   "|BEFORE|BOOL|CACHE|CLEAR|CODE|COMMAND|COMMAND_NAME|COMMENT|COMPARE"
			   "|COMPILE_DEFINITIONS|COMPILE_FLAGS|COMPONENT|CONFIGURATIONS"
			   "|COPYONLY|DEFINED|DEPENDS|DESTINATION|DIRECTORY|DOC|ENV|EQUAL"
			   "|ERROR_FILE|ERROR_QUIET|ERROR_VARIABLE|ESCAPE_QUOTES|EXCLUDE"
			   "|EXCLUDE_FROM_ALL|EXISTS|EXPORT|EXT|EXTRA_INCLUDE|FALSE|FATAL_ERROR|FILE"
			   "|FILEPATH|FILES|FILES_MATCHING|FORCE|FUNCTION|GENERATED|GET|GLOB|GLOB_RECURSE|GREATER"
			   "|GROUP_SIZE|HEADER_FILE_ONLY|HEADER_LOCATION|HINTS|INCLUDES"
			   "|INCLUDE_INTERNALS|INPUT_FILE|INSERT|INTERNAL|IS_DIRECTORY|LENGTH"
			   "|LESS|LIBRARY|LOCATION|MACOSX_BUNDLE|MAIN_DEPENDENCY|MAKE_DIRECTORY"
			   "|MATCH|MATCHALL|MATCHES|MODULE|NAME|NAMES|NAME_WE|NO|NOT|NOTEQUAL|NOTFOUND"
			   "|NO_CMAKE_ENVIRONMENT_PATH|NO_CMAKE_PATH|NO_CMAKE_SYSTEM_PATH"
			   "|NO_DEFAULT_PATH|NO_SYSTEM_ENVIRONMENT_PATH|OBJECT_DEPENDS|OFF|ON"
			   "|OPTIONAL|OR|OUTPUT|OUTPUT_FILE|OUTPUT_QUIET|OUTPUT_VARIABLE|PASS_REGULAR_EXPRESSION|PATH"
			   "|PATHS|PATH_SUFFIXES|PATTERN|PERMISSIONS|POST_BUILD|PREORDER|PRE_BUILD"
			   "|PRE_LINK|PROGRAM|PROGRAMS|PROPERTIES|RANGE|READ|READ_WITH_PREFIX|REGEX"
			   "|REGULAR_EXPRESSION|RELATIVE|RELATIVE_PATH|REMOVE|REMOVE_AT"
			   "|REMOVE_ITEM|REMOVE_RECURSE|RENAME|REPLACE|REQUIRED|RESULT_VARIABLE"
			   "|RETURN_VALUE|REVERSE|RUNTIME|RUNTIME_DIRECTORY|SCRIPT|SEND_ERROR"
			   "|SHARED|SORT|STATIC|STATUS|STREQUAL|STRGREATER|STRING|STRLESS"
			   "|SUBSTRING|TARGET|TARGETS|TIMEOUT|TOLOWER|TOUPPER|TO_CMAKE_PATH"
			   "|TO_NATIVE_PATH|TRUE|TYPE|WORKING_DIRECTORY|WRAP_EXCLUDE|WRITE"
			   "|debug|optimized|general|", p);
	if (found) {
		/* Make sure that it is the same case as in the list above.
		 * This will help reduce the number of false positive
		 * detections */
		if (!strncmp(p, found, strlen(p))) {
			return 1;
		} else {
			return 0;
		}
	} else {
		return 0;
	}
}

is_cmake_variable(char *p) {
	/* This list obtained by running:
	 * cmake --help-variable-list | tr -d "\r" | tr "\n" "|"
	 * */
	if (strstr("|CMAKE_AR|CMAKE_BINARY_DIR|CMAKE_BUILD_TOOL"
			   "|CMAKE_CACHEFILE_DIR|CMAKE_CACHE_MAJOR_VERSION"
			   "|CMAKE_CACHE_MINOR_VERSION|CMAKE_CACHE_RELEASE_VERSION"
			   "|CMAKE_CFG_INTDIR|CMAKE_COMMAND|CMAKE_CROSSCOMPILING"
			   "|CMAKE_CTEST_COMMAND|CMAKE_CURRENT_BINARY_DIR"
			   "|CMAKE_CURRENT_LIST_FILE|CMAKE_CURRENT_LIST_LINE"
			   "|CMAKE_CURRENT_SOURCE_DIR|CMAKE_DL_LIBS"
			   "|CMAKE_EDIT_COMMAND|CMAKE_EXECUTABLE_SUFFIX"
			   "|CMAKE_GENERATOR|CMAKE_HOME_DIRECTORY"
			   "|CMAKE_IMPORT_LIBRARY_PREFIX"
			   "|CMAKE_IMPORT_LIBRARY_SUFFIX|CMAKE_LINK_LIBRARY_SUFFIX"
			   "|CMAKE_MAJOR_VERSION|CMAKE_MAKE_PROGRAM"
			   "|CMAKE_MINOR_VERSION|CMAKE_PARENT_LIST_FILE"
			   "|CMAKE_PATCH_VERSION|CMAKE_PROJECT_NAME|CMAKE_RANLIB"
			   "|CMAKE_ROOT|CMAKE_SHARED_LIBRARY_PREFIX"
			   "|CMAKE_SHARED_LIBRARY_SUFFIX|CMAKE_SHARED_MODULE_PREFIX"
			   "|CMAKE_SHARED_MODULE_SUFFIX|CMAKE_SIZEOF_VOID_P"
			   "|CMAKE_SKIP_RPATH|CMAKE_SOURCE_DIR"
			   "|CMAKE_STANDARD_LIBRARIES|CMAKE_STATIC_LIBRARY_PREFIX"
			   "|CMAKE_STATIC_LIBRARY_SUFFIX|CMAKE_USING_VC_FREE_TOOLS"
			   "|CMAKE_VERBOSE_MAKEFILE|CMAKE_VERSION"
			   "|PROJECT_BINARY_DIR|PROJECT_NAME|PROJECT_SOURCE_DIR"
			   "|BUILD_SHARED_LIBS|CMAKE_BACKWARDS_COMPATIBILITY"
			   "|CMAKE_BUILD_TYPE|CMAKE_COLOR_MAKEFILE"
			   "|CMAKE_CONFIGURATION_TYPES|CMAKE_FIND_LIBRARY_PREFIXES"
			   "|CMAKE_FIND_LIBRARY_SUFFIXES|CMAKE_INCLUDE_PATH"
			   "|CMAKE_INSTALL_PREFIX|CMAKE_LIBRARY_PATH|CMAKE_MFC_FLAG"
			   "|CMAKE_MODULE_PATH|CMAKE_NOT_USING_CONFIG_FLAGS"
			   "|CMAKE_PREFIX_PATH|CMAKE_PROGRAM_PATH"
			   "|CMAKE_SYSTEM_INCLUDE_PATH|CMAKE_SYSTEM_LIBRARY_PATH"
			   "|CMAKE_SYSTEM_PREFIX_PATH|CMAKE_SYSTEM_PROGRAM_PATH"
			   "|CMAKE_USER_MAKE_RULES_OVERRIDE|APPLE|BORLAND|CMAKE_CL_64"
			   "|CMAKE_COMPILER_2005|CMAKE_HOST_APPLE|CMAKE_HOST_SYSTEM"
			   "|CMAKE_HOST_SYSTEM_NAME|CMAKE_HOST_SYSTEM_PROCESSOR"
			   "|CMAKE_HOST_SYSTEM_VERSION|CMAKE_HOST_UNIX|CMAKE_HOST_WIN32"
			   "|CMAKE_OBJECT_PATH_MAX|CMAKE_SYSTEM|CMAKE_SYSTEM_NAME"
			   "|CMAKE_SYSTEM_PROCESSOR|CMAKE_SYSTEM_VERSION|CYGWIN|MSVC"
			   "|MSVC80|MSVC_IDE|MSVC_VERSION|UNIX|WIN32"
			   "|CMAKE_ARCHIVE_OUTPUT_DIRECTORY|CMAKE_BUILD_WITH_INSTALL_RPATH"
			   "|CMAKE_DEBUG_POSTFIX|CMAKE_EXE_LINKER_FLAGS"
			   "|CMAKE_Fortran_MODULE_DIRECTORY"
			   "|CMAKE_INSTALL_NAME_DIR|CMAKE_INSTALL_RPATH"
			   "|CMAKE_INSTALL_RPATH_USE_LINK_PATH|CMAKE_LIBRARY_OUTPUT_DIRECTORY"
			   "|CMAKE_LIBRARY_PATH_FLAG|CMAKE_LINK_DEF_FILE_FLAG"
			   "|CMAKE_LINK_LIBRARY_FILE_FLAG|CMAKE_LINK_LIBRARY_FLAG"
			   "|CMAKE_RUNTIME_OUTPUT_DIRECTORY|CMAKE_SKIP_BUILD_RPATH"
			   "|CMAKE_USE_RELATIVE_PATHS|EXECUTABLE_OUTPUT_PATH"
			   "|LIBRARY_OUTPUT_PATH"
			   "|CMAKE_INTERNAL_PLATFORM_ABI|", p)) {
		return 1;
	} else {
		char q[1000];
		/* There are some special "wildcard" type variables we need to
		 * check for as well. */

		/* First we will remove the bookends: | |
		 * so that we can search easier */
		strncpy(q, p + 1, strlen(p) - 2);

		if (fpatmatch(q,
					  "*_BINARY_DIR|"
					  "*_SOURCE_DIR|"
					  "CMAKE_EXE_LINKER_FLAGS_*|"
					  "CMAKE_*_ARCHIVE_APPEND|"
					  "CMAKE_*_ARCHIVE_CREATE|"
					  "CMAKE_*_ARCHIVE_FINISH|"
					  "CMAKE_*_COMPILER|"
					  "CMAKE_*_COMPILER_ABI|"
					  "CMAKE_*_COMPILER_ID|"
					  "CMAKE_*_COMPILE_OBJECT|"
					  "CMAKE_*_CREATE_SHARED_LIBRARY|"
					  "CMAKE_*_CREATE_SHARED_MODULE|"
					  "CMAKE_*_CREATE_STATIC_LIBRARY|"
					  "CMAKE_*_FLAGS_DEBUG|"
					  "CMAKE_*_FLAGS_MINSIZEREL|"
					  "CMAKE_*_FLAGS_RELEASE|"
					  "CMAKE_*_FLAGS_RELWITHDEBINFO|"
					  "CMAKE_*_IGNORE_EXTENSIONS|"
					  "CMAKE_*_IMPLICIT_INCLUDE_DIRECTORIES|"
					  "CMAKE_*_LINKER_PREFERENCE|"
					  "CMAKE_*_LINK_EXECUTABLE|"
					  "CMAKE_*_OUTPUT_EXTENSION|"
					  "CMAKE_*_PLATFORM_ID|"
					  "CMAKE_*_SIZEOF_DATA_PTR|"
					  "CMAKE_*_SOURCE_FILE_EXTENSIONS|"
					  "CMAKE_COMPILER_IS_GNU*|"
					  "CMAKE_USER_MAKE_RULES_OVERRIDE_*|"
					  "CMAKE_*_POSTFIX", 0, 0)) {
			return 1;
		} else {
			return 0;
		}
	}
}
