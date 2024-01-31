---
title:                "Видалення символів за візерунком"
date:                  2024-01-20T17:41:40.733851-07:00
model:                 gpt-4-1106-preview
simple_title:         "Видалення символів за візерунком"

category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (## Що і Чому?)
Deleting characters that match a pattern means filtering out unwanted characters from strings. Programmers do it to sanitize, parse, or format data effectively.

## How to: (## Як це зробити:)
Let's delete vowels from a string as an example. Here's the code and what it spits out.

```C
#include <stdio.h>
#include <string.h>

void delete_pattern(char *str, const char *pattern) {
    char *output = str;
    for (; *str != '\0'; str++) {
        const char *temp;
        for (temp = pattern; *temp != '\0'; temp++) {
            if(*str == *temp) break;
        }
        if(*temp == '\0') {
            *output++ = *str;
        }
    }
    *output = '\0';
}

int main() {
    char myString[] = "Hello, World!";
    delete_pattern(myString, "aeiouAEIOU");
    printf("Result: %s\n", myString); // Prints "Hll, Wrld!"
    return 0;
}
```

## Deep Dive (## Поглиблене Вивчення)
This simple function has roots in the C standard libraries like `strpbrk` but gives us more control. Alternatives include regex but that's overkill for simple patterns. The implementation above is a clear illustration: it goes through each character and checks it against the pattern, preserving only the non-matching ones.

The function can be expanded to handle wide characters or more complex patterns. It's lean and mean, with no extra fluff. This type of function is useful for text processing where patterns are known and regular expressions are unnecessary heavy artillery.

## See Also (## Дивіться Також)
- C Standard Library: http://www.cplusplus.com/reference/clibrary/
- Regular expressions in C: https://www.regular-expressions.info/c.html
- Text processing in C: https://www.oreilly.com/library/view/c-cookbook/0596006977/ch04.html

These resources will help you learn more. The first explains basic C functions. The second dives into regex if you need more power. The third offers practical recipes for text processing. Happy coding!
