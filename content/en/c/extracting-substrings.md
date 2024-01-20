---
title:                "Extracting substrings"
html_title:           "Arduino recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?

Extracting substrings is a frequent operation in programming where a portion of a string is isolated for use. Why do it? Two reasons: (1) to manipulate parts of a string separately, and (2) to filter information from a larger string content.

## How to:

In C, you often use the `strncpy()` function from the `<string.h>` library. Here's a simple example of how you'd extract a substring:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char original_string[50] = "Hello, World!";
    char target_string[10];

    strncpy(target_string, original_string + 7, 5);
    target_string[5] = '\0'; // Null-terminating the string

    printf("%s\n", target_string);
    return 0;
}
```

The expected output here would be `World`.

## Deep Dive

Historically, C didn't have a native function for substring extraction, hence the use of `strncpy()`. Alternatives include pointer arithmetic and character copying using loops, but `strncpy()` remains a straightforward and common method.

When using `strncpy()`, don't forget to null-terminate the target string manually. The function does **not** append a null character at the end of the copied substring, potentially leading to unexpected results by reading beyond the target string.

## See Also

For more details, check out [cstring reference](http://www.cplusplus.com/reference/cstring/) and [strncpy() on other languages](https://stackoverflow.com/q/4214314/4231057). Refer to [The GNU C Programming Tutorial](http://www.crasseux.com/books/ctutorial/) for both beginners and experienced programmers.