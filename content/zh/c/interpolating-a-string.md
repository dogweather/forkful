---
title:                "字串插值"
html_title:           "C: 字串插值"
simple_title:         "字串插值"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/interpolating-a-string.md"
---

{{< edit_this_page >}}

# 什么和为什么？

字符串插值是指通过将变量值插入到字符串中来创建新的字符串。程序员们会这样做是为了能够更容易、更高效地操作字符串。它也可以使代码更易读，并提高可维护性。

# 如何：

```C
#include <stdio.h>

int main() {
    int num = 10;
    char name[] = "John";

    // 字符串插值实例
    printf("我的名字是 %s，我今年 %d 岁。\n", name, num );

    return 0;
}
```

输出：

```bash
我的名字是 John，我今年 10 岁。
```

# 深入探讨：

1. 历史背景：字符串插值在早期的编程语言中并不常见，但随着语言的发展，它变得越来越普遍。现在，许多流行的编程语言都支持字符串插值。

2. 其他选择：除了字符串插值，程序员们还可以使用字符串连接来拼接字符串。然而，字符串插值通常更简洁、更直观。

3. 实现细节：在C语言中，字符串插值可以通过`printf()`函数来实现，它接受一个格式化字符串作为参数，并将变量的值插入到对应的位置。

# 参考资料：

- [字符串插值在不同编程语言中的差异](https://www.itcodemonkey.com/article/10373.html)

- [如何正确使用字符串插值来提高代码的可读性和可维护性](https://www.caster.io/courses/strings-insertion-guide/lesson/1)

- [C语言中的字符串插值技巧](https://www.itcodemonkey.com/article/4250.html)