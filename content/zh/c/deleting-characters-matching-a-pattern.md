---
title:                "删除匹配模式的字符"
html_title:           "Java: 删除匹配模式的字符"
simple_title:         "删除匹配模式的字符"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 什么是模式匹配删除字符？为什么程序员这么做？
删除匹配模式的字符是指从字符串中删除符合特定模式的字符。程序员之所以进行这样的操作，主要是为了清理不需要的数据或进行字符串格式化。

## 怎么做：
让我们看一个实现字符模式匹配删除的例子。
```C
#include <string.h>
#include <stdio.h>

void del_chars(char *str, const char *chars_to_delete) {
    for (int i = 0; str[i]; i++) {
        for (int j = 0; chars_to_delete[j]; j++) {
            if (str[i] == chars_to_delete[j]) {
                memmove(&str[i], &str[i + 1], strlen(str) - i);
            }
        }
    }
}

int main() {
    char str[] = "Hello, World!";
    del_chars(str, "l");
    printf("%s\n", str);
    return 0;
}
```
以上的代码将从字符串 "Hello, World!" 删除所有的 "l"，输出："Heo, Word!"。

## 深入探究
1. 历史背景：在早期的计算机编程语言如 Fortran 和 COBOL 中，字符串操作并不常见。但随着时间的推移，字符串操作尤其是对字符串中字符的删除变得越来越重要。在 C 语言中，我们可以使用内置函数 memmove 来实现这种功能（如上述代码示例）。
2. 替代方法：还有其他方式可以实现字符模式匹配删除的目标，例如使用库函数 strpbrk 或 strtok。但是，上述实现的优点在于它没有使用额外的内存空间。
3. 实现细节：在上述例子中，我们遍历了给定字符串，对比每个字符看是否需要删除。如果需要删除，我们就使用 memmove 将后续的字符向前移动一个位置。

## 参见
- [Wikipedia: String operations](https://en.wikipedia.org/wiki/String_operations)
- [CPlusPlus: Memmove](http://www.cplusplus.com/reference/cstring/memmove/)
- [TutorialsPoint: C - Strings](https://www.tutorialspoint.com/cprogramming/c_strings.htm)