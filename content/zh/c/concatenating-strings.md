---
title:                "字符串拼接"
date:                  2024-01-20T17:34:05.009872-07:00
model:                 gpt-4-1106-preview
simple_title:         "字符串拼接"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (什么与为什么？)
字符串拼接是将两个或多个字符串连接成一个新的字符串。程序员这么做是为了创建动态的输出信息，或者构建具有变动部分的字符串数据。

## How to: (如何做？)
在C语言中，用标准库函数`strcat()`实现字符串拼接。例如：
```C
#include <stdio.h>
#include <string.h>

int main() {
    char source[20] = "世界";
    char destination[50] = "你好，";

    strcat(destination, source); // 拼接字符串
    printf("%s\n", destination); // 输出拼接后的结果

    return 0;
}
```
输出:
```
你好，世界
```

## Deep Dive (深入探讨)
在C的历史中，字符串自始至终都是以字符数组的形式存在的，原因是C标准并没有内建的字符串类型。`strcat()`函数自C标准库出现以来就是拼接字符串的标准方法。此外，还有`strncat()`，它允许限制添加到目标字符串的最大字符数，这是有效避免缓冲区溢出的一种方式。

实现细节包括确保目标字符串足够大以容纳结果，及`strcat()`在拼接前不会检查溢出。因此，`strncat()`更安全一些。

另一种选择是使用`sprintf()`或`snprintf()`进行拼接，它们可以处理不同类型的数据，并将其格式化为字符串。

## See Also (另请参阅)
- C标准库参考：[http://www.cplusplus.com/reference/cstring/](http://www.cplusplus.com/reference/cstring/)
- 关于`sprintf`和`snprintf`的使用：[http://www.cplusplus.com/reference/cstdio/sprintf/](http://www.cplusplus.com/reference/cstdio/sprintf/)