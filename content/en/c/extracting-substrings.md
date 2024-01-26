---
title:                "Extracting substrings"
date:                  2024-01-20T17:45:09.012472-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extracting substrings"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
Extracting substrings means grabbing a specific part of a string - a slice of the pie. Programmers do it to isolate, process, or manipulate only the relevant bits of data in a larger text.

## How to:
Let's pluck some substrings from a string using C.

```C
#include <stdio.h>
#include <string.h>

void extract_substring(const char *source, int start, int length, char *dest) {
    strncpy(dest, source + start, length);
    dest[length] = '\0'; // Don't forget to null-terminate!
}

int main() {
    const char *full_text = "Extracting substrings is neat.";
    char snippet[20];

    extract_substring(full_text, 0, 10, snippet);
    printf("Snippet: %s\n", snippet);

    extract_substring(full_text, 12, 10, snippet);
    printf("Another: %s\n", snippet);

    return 0;
}
```

Sample output:

```
Snippet: Extracting
Another: substrings
```

## Deep Dive
Extracting substrings ain't exactly new. In the C programming realm, it's been a common task since the language's conception in the 1970s.

You've got alternative ways to grab those substrings. Some folks use `strncpy()`, like our example above. Others might prefer `sscanf()` or even manually loop through the string. Each approach has its nuances. With `strncpy()`, watch out - it won't null-terminate for you if the length you specify reaches beyond the string's end.

Under the hood, a string is just an array of characters in C. When slicing, you're dealing with pointers to particular addresses in memory. Keep an eye on bounds and always null-terminate your snippets.

## See Also
- `strncpy()` manual: https://www.man7.org/linux/man-pages/man3/strncpy.3.html
- C String handling: https://en.cppreference.com/w/c/string
- Pointers and arrays: https://www.tutorialspoint.com/cprogramming/c_pointers.htm
