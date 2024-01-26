---
title:                "Finding the length of a string"
date:                  2024-01-20T17:46:54.826336-07:00
model:                 gpt-4-1106-preview
simple_title:         "Finding the length of a string"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Finding a string's length means counting the number of characters it contains before the null terminator `\0`. It's crucial for manipulating strings—like when we need to loop through a string or allocate exact memory spaces.

## How to:

Your go-to in C for measuring string length is the `strlen` function from `<string.h>`. Here's how it works:

```c
#include <stdio.h>
#include <string.h>

int main() {
    char myString[] = "Hello, World!";
    size_t length = strlen(myString);  // Use size_t for string length
    printf("The length of '%s' is %zu.\n", myString, length);
    return 0;
}
```

Expected output:
```
The length of 'Hello, World!' is 13.
```

Remember, `strlen` doesn't count the null terminator.

## Deep Dive

Historically, strings in C end with a `\0` null character—this is how functions know where a string finishes. Fun fact: `strlen` itself is a simple loop running from the start of the string to the null terminator.

What if `strlen` isn't your jam? For embedded systems or performance-critical code, you might write a custom length function to avoid library overhead or to handle non-standard string formats. Just tread carefully; it's a bug fiesta if done wrong.

Under the hood, `strlen` can vary between simple and sophisticated. The naïve implementation could be just a few lines of code in a loop, whereas optimized versions may employ techniques like loop unrolling or parallel processing to speed things up on large strings.

## See Also

For those hungry for more, feast on these:

- C Standard Library reference for `strlen`: [https://www.cplusplus.com/reference/cstring/strlen/](https://www.cplusplus.com/reference/cstring/strlen/)
- Deep dive into how strings work in C: [https://www.cs.swarthmore.edu/~newhall/unixhelp/C_strings.html](https://www.cs.swarthmore.edu/~newhall/unixhelp/C_strings.html)
- For a challenge, read about optimizing string functions: [https://opensource.com/article/19/5/how-write-good-c-main-function](https://opensource.com/article/19/5/how-write-good-c-main-function)
