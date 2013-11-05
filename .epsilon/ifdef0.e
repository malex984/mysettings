/* IFDEF0.E
 *
 * This extension will color all code between a #if and it's matching
 * #else or #endif in the comment color.  It'll also color the first
 * #else or #elif after a #if 1 in the comment color.  It doesn't handle
 * "#elif 0" (unless it happens to follow immediately after a #if 1).
 *
 * It'll color the block properly whenever it has to color the if or
 * endif.  When typing inside of one of these blocks, or when changing
 * the value in the if (from 0 to 1 or vice-versa) the coloring may not
 * be correct.  This can usually be fixed by modifying the line with the
 * endif on it.
 *
 * It seems to work pretty well if the #if and the #else or #endif (I'll
 * just refer to the matching #elif, #else or #endif after the #if as an
 * endif) are close enough.  What's "close enough"?  That seems to be
 * affected by several factors, the largest seems to be whether the
 * endif is visible on the screen or not.  If it's off by more than a
 * line or two then the comment coloring may disappear and reappear if
 * you type inside the if/endif block.
 *
 * If you don't edit any of the code in the if block, then it should get
 * colored properly and stay that way.
 *
 * NOTE:  This will not recolor blocks that have already been colored at
 * the time that this was first compiled.  (To do this, save the state,
 * exit Epsilon, and reload the files; disable then reenable code coloring
 * in the buffer(s); or modify the line with the "#if" on it (hit a space
 * on it, then undo it.)  When this is saved with the state, this won't
 * be a problem.
 *
 * If you don't like the results after trying this out, you can put it
 * back the old way by either compiling COLCODE.E (from in the SOURCE
 * directory) or by changing the define of TRY_COLORING_IF0 to zero in
 * IFDEF0.E and recompiling that.
 *
 * Future enhancements:
 * 	Proper handling of #elif clauses
 * 	Handling multiple #elif clauses
 *
 * Wish list:
 * 	Support for defines - recognize something like "#if TRY_THIS" and
 * 	color it appropriately based on the value for TRY_THIS.
 *
 * 	Support for handling "#ifdef" and related constructs.
 *
 *
 * This code is dedicated to the public domain with the caveat that Lugaru is
 * welcome to use this within their distribution source code which is
 * supplied with Epsilon.
 *
 * Glenn Dill
 * gdill@icubed.com
 */

#include "eel.h"
#include "c.h"
#include "colcode.h"

// Make this a zero to disable coloring if "#if 0" blocks of code
#define TRY_COLORING_IF0 1

// Redefine color_c_range (from COLCODE.E)
color_c_range(from, to) // recolor just this section
{			// last colored region may go past to
	int t = -1, talk, s;
	char pat[200];

	if (from >= to)
		return to;
	save_var point, matchstart, matchend;
	c_init_color(from, to);
	point = from;
	talk = (to - from > 2000);	// show status during long delays
	save_var case_fold = 0;
	strcpy(pat, "/<*>|//|^[ \t]*#|[\"']");
	if (!minimal_coloring)
		strcat(pat, "|[A-Za-z_][A-Za-z0-9_]*"
			   "|-?%.?[0-9]([A-Za-z0-9._]|[Ee]-)*");
	while (point < to) {
		if (!re_search(1, pat)) {
			t = size();
			break;
		}
		t = matchstart;
		switch (character(point - 1)) {		// check last char
			case '/':			// found // one-line comment
				while (nl_forward() && character(point - 2) == '\\')
					;		// Continued // comment.
				set_character_color(t, point, color_class c_comment);
				break;
			case '*':			// found /* starting comment
				search(1, "*/");
				set_character_color(t, point, color_class c_comment);
				break;
			case '#':			// found preproc line
#if TRY_COLORING_IF0
				if (c_color_if_block()) break;
#endif
				c_preproc_color();	// move past correct part
				set_character_color(t, point,
									color_class c_preprocessor);
				break;
			case '"':		// found a string literal
				point = t;
				re_search(1, "\"([^\"\\\n]|\\(.|\n))*[\"\n]");
				set_character_color(t, point, color_class c_string);
				if (get_character_color(point, (int *) 0, &s) == 
					color_class c_string && s > to)  // fix up after
					c_init_color(point, to = s); // quoted "'s
				break;
			case '\'':		// found a char const
				point = t;
				re_search(1, "\'([^\'\\\n]|\\(.|\n))*[\'\n]");
				set_character_color(t, point, color_class c_charconst);
				break;
			default:		// found identifier, kywd, or number
				set_character_color(t, point, c_keyword_color(t));
				break;
		}
		if (talk)
			note("Coloring C program: %d%% complete...",
				 (point - from) * 100 / (to - from));
	}
	c_init_color(to, t);
	if (talk)
		note("");
	return point;
}

#if TRY_COLORING_IF0
// Based on stuff in C.E
// #define PREPROC_LINE	"^[ \t]*#[ \t]*(if|else|endif|elif|asm)"
// #define PREPROC_IF		"^[ \t]*#[ \t]*(if|asm)"
// #define PREPROC_ELSE	"^[ \t]*#[ \t]*(else|elif)"
// #define PREPROC_ENDIF	"^[ \t]*#[ \t]*(endif|endasm)"


// #define PREPROC_LINE	"^[ \t]*#[ \t]*(if|else|endif|elif)"
// #define PREPROC_IF		"^[ \t]*#[ \t]*(if)"
// #define PREPROC_ELSE	"^[ \t]*#[ \t]*(else|elif)"
// #define PREPROC_ENDIF	"^[ \t]*#[ \t]*(endif)"

#define PREPROC_IFELIF	"^[ \t]*#[ \t]*(if|elif)"




// returns 1 if found, 0 if not.  Assumes that you're sitting on a preproc line
int find_matching_preproc(int dir) {
	int level = 0;
	if (dir > 0) to_end_line();
	else to_begin_line();
	for (;;) {		// Look for a preprocessor line at the right level.
		if (!re_search(dir, PREPROC_LINE)) {
			return 0;		// Unmatched preproc
		}
		if (parse_string(-dir, PREPROC_IF)) {
			if ((dir > 0) ? (++level <= 0) : (++level > 0))
				break;
		} else if (parse_string(-dir, PREPROC_ENDIF)) {
			if ((dir > 0) ? (--level < 0) : (--level >= 0))
				break;
		} else if (parse_string(-dir, PREPROC_ELSE)) {
			if ((dir > 0) ? (level <= 0) : (level >= 0))
				break;
		}
	}
	to_begin_line();
	return 1;
}

color_if0() {
	int start = point;
	//re_search(1, PREPROC_IF);
	re_search(1, PREPROC_IFELIF);
	//set_character_color(start, point, color_class /*c_preprocessor*/menu_highlight);
	set_character_color(start, point, color_class /*menu_highlight*/c_preprocessor);
	re_search(1, "[ \t]+");
	set_character_color(point, point + 1, color_class c_number);
	point++;
}

#if 0
color_endif() {
	int start = point;
	re_search(1, PREPROC_ENDIF);
	set_character_color(start, point, color_class c_preprocessor);
}
#endif

// returns 0 - keep coloring as normal
// 1 - colored it as comment
int c_color_if_block() {
	// point is sitting on a '#' character
	int orig = point;
	re_search(1, "[ \t]*");
	if (parse_string(1, "(el)?""if[ \t]+0([ \t]|<newline>)")) {
		// It's a #if 0 or #elif 0
		int start;
		//point = orig - 1;
		to_begin_line();
		color_if0();
		start = point;
		if (find_matching_preproc(1)) {
#if 0
			int len;
			len = parse_string(1, "[ \t]*#[ \t]*endif");
			if (len) point += len;
			set_character_color(orig - 1, point, color_class c_comment);
#else
			set_character_color(start, point, color_class c_comment);
			// point is currently at the beginning of the line with the
			// matching #else, #elif, or #endif
			start = point;
#if 0
			if (parse_string(1, PREPROC_ENDIF)) {
				nl_forward();
				set_character_color(start, point, color_class c_preprocessor);
				return 1;
			}
			// It's some kind of #else
#endif
			re_search(1, PREPROC_LINE);
			set_character_color(start, point, color_class c_preprocessor);
#endif
			return 1;
		}
		point = orig;
		return 0;
	}
	if (parse_string(1, "elif|else|endif")) {
		int start = orig;
		//char found_one = 0;
		while (find_matching_preproc(-1)) {
			check_abort();
			if (parse_string(1, PREPROC_IF)) {
				if (parse_string(1, PREPROC_IFELIF"[ \t]+0[ \t]*$")) {
					color_if0();
					nl_forward();
					set_character_color(point, start, color_class c_comment);
					//found_one = 1;
				} else if (parse_string(1, PREPROC_IFELIF"[ \t]+1[ \t]*$")) {
					color_if0();
					// This should be a while?
					if (find_matching_preproc(1)) {
						start = point;
						if (parse_string(1, PREPROC_ENDIF)) {
							nl_forward();
							set_character_color(start, point, color_class c_preprocessor);
						} else {
							if (parse_string(1, "^[ \t]*#[ \t]*(elif)")) {
								color_if0();
								nl_forward();
							} else {
								nl_forward();
								set_character_color(start, point, color_class c_preprocessor);
							}
							start = point;
							if (find_matching_preproc(1)) {
								to_begin_line();
								set_character_color(start, point, color_class c_comment);
							}
						}
					}
					//found_one = 2;
				} //else found_one = 3;
				if (point < orig) {
					point = orig;
					return 0;
				}
				return 1;
			}
#if 0
			if (found_one == 1 && parse_string(1, "^[ \t]*#[ \t]*(else|elif)")) {
				// Found one to color normally
			}
			if (!found_one) {
				// We're moving backwards, so color the gray then color the if
				int here = point;
				//color_if0();
				nl_forward();
				set_character_color(point, start, color_class c_comment);
				point = here;
			}
			if (parse_string(1, PREPROC_ENDIF)) {
				color_endif();
				if (point < orig) {
					point = orig;
					return 0;
				}
				return 1;
			}
#endif
			start = point;
		}
		point = orig;
	}
	return 0;
}
#endif
