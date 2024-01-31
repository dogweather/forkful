---
title:                "패턴에 일치하는 문자 삭제"
date:                  2024-01-20T17:41:41.524313-07:00
model:                 gpt-4-1106-preview
simple_title:         "패턴에 일치하는 문자 삭제"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
패턴에 맞는 문자 삭제하기란 문자열에서 특정 패턴을 찾아 그것들을 없애는 과정입니다. 프로그래머들은 불필요한 데이터를 정리하거나 입력을 필터링할 때 이 기능을 사용합니다.

## How to: (어떻게:)
```C
#include <stdio.h>
#include <string.h>

void delete_pattern(char *str, const char *pattern) {
    char *src = str, *dest = str;
    while (*src) {
        const char *temp_pat = pattern;
        while (*temp_pat && *src != *temp_pat) ++temp_pat;
        if (!*temp_pat) *dest++ = *src;
        src++;
    }
    *dest = '\0';
}

int main() {
    char str[] = "Hello, World! It's a brand new day!";
    const char *pattern = "aeiou";
    delete_pattern(str, pattern);
    printf("After deleting vowels: %s\n", str);
    return 0;
}
```
Sample Output:
```
After deleting vowels: Hll, Wrld! It's  brnd nw dy!
```

## Deep Dive (심화 학습):
Deleting characters matching a pattern can be traced back to the early days of Unix, where utilities like `tr` and `sed` provided similar functionalities. In C, you can do it without external tools. The function above iterates through each character in the source string, checks against a pattern, and copies it to the destination if it doesn’t match.

Alternatives include using regular expressions with libraries such as `regex.h`, but that could be overkill for simple patterns. If performance concerns arise, more sophisticated algorithms like Knuth-Morris-Pratt or Boyer-Moore could be implemented for pattern searching and deletion.

To avoid modifying the original string, you might create a new string to store the result. Remember to allocate enough memory for the new string and to free it appropriately to prevent memory leaks.

## See Also (참고 자료):
- C Standard Library documentation (http://www.cplusplus.com/reference/clibrary/)
- Regular Expressions in C (https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html)
- Optimizing string algorithms (http://www-igm.univ-mlv.fr/~lecroq/string/index.html)
