---
title:                "将字符串转换为小写"
html_title:           "Arduino: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 什么和为什么？
将字符串转换为小写是在程序中将所有大写字母更改为其小写形式的过程。程序员这样做主要是为了实现数据的规范化，方便进行字符串比较和排序。

## 如何操作：
我们可以使用C语言的`tolower()`函数来实现这一转换。这里有一个示例：

```C
#include <ctype.h>
#include <stdio.h>

int main() {
    char str[] = "HELLO, WORLD!";

    for (int i = 0; str[i]; i++) {
        putchar(tolower(str[i]));
    }

    return 0;
}
```

运行上述程序，你会在控制台上看到以下输出：

```C
hello, world!
```
这样，我们就成功地将字符串转换为小写了。

## 深入分析
1. 历史背景：最早的C标准库中就已经包含了`tolower()`函数，这显示了数据规范化的重要性。
2. 替代方案：你也可以不使用`tolower()`函数，而是直接使用ASCII值进行大小写转换。
3. 实现细节：`tolower()`函数通过查找ASCII表来进行转换。它检查字符是否为大写，如果是，则将其ASCII值加32（因为在ASCII表中，小写字母的ASCII值比其对应的大写字母大32）。

## 另请参阅
1. C编程/字符：更深层次了解字符和字符串的知识（地址：www.C-lang-char.com)
2. ASCII表参考：了解ASCII的各种细节（地址：www.ASCII-ref.com)
3. C标准库 - `<ctype.h>`：读懂`tolower()`函数的官方文档（地址：www.ctype-h-lib.com)