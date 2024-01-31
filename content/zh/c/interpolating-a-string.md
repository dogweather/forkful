---
title:                "字符串插值"
date:                  2024-01-20T17:50:13.431859-07:00
model:                 gpt-4-1106-preview
simple_title:         "字符串插值"

category:             "C"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? 什么与为什么
字符串插值是将变量值拼接进固定格式的字符串过程。程序员这么做是为了创造动态消息和易于维护代码。

## How to: 如何操作
在C语言中，你使用 `printf` 函数来插值字符串。以下是示例代码和输出结果。

```C
#include <stdio.h>

int main() {
    int age = 28;
    const char *name = "张三";

    printf("姓名：%s，年龄：%d。\n", name, age);

    return 0;
}
```

输出结果：

```
姓名：张三，年龄：28。
```

## Deep Dive 深度潜入
字符串插值在早期编程语言中不常见。C语言使用格式化打印，这比现代语言的内置插值简陋。替代方案有拼接字符串，但维护麻烦。了解底层，`printf` 把格式化字符串和后续参数结合产生最终结果。详细了解，要查阅C语言标准库文档。

## See Also 相关资源
- [C 标准库 - printf](http://www.cplusplus.com/reference/cstdio/printf/)
