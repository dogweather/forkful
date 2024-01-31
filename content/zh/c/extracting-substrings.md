---
title:                "提取子字符串"
date:                  2024-01-20T17:45:24.557226-07:00
model:                 gpt-4-1106-preview
simple_title:         "提取子字符串"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (什么 & 为什么？)
提取子串就是从一个字符串中取出一部分内容。程序员这么做是为了分析、比较或修改特定数据片段。

## How to: (如何操作：)
```C
#include <stdio.h>
#include <string.h>

int main() {
    char str[] = "Hello, World!";
    char substr[6];

    // 提取子串 "Hello"
    strncpy(substr, str, 5);
    substr[5] = '\0'; // 添加空字符'\0'以结束子串表示

    printf("Substr: %s\n", substr); // 打印 "Hello"

    return 0;
}
```
Output:
```
Substr: Hello
```

## Deep Dive (深入了解)
在C语言的历史中，处理字符串一直都是编程中的基础任务。标准库中的 `strncpy()` 函数常用于提取子串，但要注意它不会自动加上终止字符 '\0'。还可以用指针操作或 `strncat()` 实现提取，但要编写更多代码。在实现时，考虑效率和内存安全至关重要。

## See Also (另请参阅)
- C标准库文档：https://en.cppreference.com/w/c/string/byte/strncpy
- 指针与字符串：https://www.learn-c.org/en/Pointers_and_Strings
- 字符串操作的内存安全：https://owasp.org/www-community/vulnerabilities/Buffer_Overflow
