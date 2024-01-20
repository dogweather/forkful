---
title:                "Deleting characters matching a pattern"
html_title:           "Lua recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?

Deleting characters matching a pattern means removing specific character(s) based on a criterion from a character string. Programmers do this to optimize code & data, enhance readability, or customize output.

## How to:

Consider you have the string "Hello!123", and you want to delete all character '1'. Here's how you do it in C:
```C 
#include<stdio.h>
#include<string.h>

void main() {
   char str[100] = "Hello!123";
   char toRemove = '1';
   int ptr1=0, ptr2=0;
   
   while(str[ptr1])
   {
      if(str[ptr1] != toRemove)
      {
         str[ptr2] = str[ptr1];
         ptr2++;
      }
      ptr1++;
   }
   str[ptr2] = '\0';
   
   printf("%s", str);
}
```
This'll output: `Hello!23` as '1' is deleted.

## Deep Dive:

Deleting characters matching a pattern first appeared in early text parsing & manipulation languages like AWK, PERL in the '70s-'80s before entering mainstream languages like Python, Java, C etc.
 
Alternatives to this in C could be using external libraries such as GLib with simplified APIs like g_strdelimit(). However, any added libraries create dependencies that may not be optimal.

In our C code, we're working with pointers & manual string traverse. The potential no-match characters are pushed to the beginning of the same string and '\0' marks the end. This ensures no extra memory is used, upholding C's low-level nature and memory efficiency.

## See Also:

1. [C Programming Guide](https://www.cprogramming.com/)
2. [String Manipulations In C Programming Using Library Functions](https://www.includehelp.com/c-programs/string-manipulation.aspx)
3. [GLib String Manipulation Functions](https://developer.gnome.org/glib/stable/glib-String-Utility-Functions.html)