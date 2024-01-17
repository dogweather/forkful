---
title:                "将字符串转换为小写"
html_title:           "C: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# 什么和为什么？
将字符串转换为小写是一种常见的编程操作，它可以把字符串中的所有大写字母转换为小写字母。程序员经常使用这种操作来简化字符串比较以及对用户输入的处理。

# 如何实现：
```C
#include <stdio.h>
#include <ctype.h>

int main() {
    // 示例字符串
    char str[] = "HELLO WORLD";
    int i = 0;

    // 使用循环将字符串中的每个字符转换为小写
    while (str[i]) {
        str[i] = tolower(str[i]);
        i++;
    }

    // 打印转换后的字符串
    printf("转换后的字符串为：%s", str);

    return 0;
}
```
输出结果：hello world

# 深入了解：
- 历史背景：在早期的计算机系统中，大写字母和小写字母被分别存储在不同的字符集中，因此需要进行大小写转换。而在现代的编程语言中，大小写转换也被广泛应用于字符串比较和用户输入处理。
- 其他方法：除了使用循环和库函数如`tolower()`之外，也可以使用条件语句和位运算来实现字符串转换。
- 实现细节：在C语言中，每个字符都是以ASCII码来表示的，在进行大小写转换时，只需要将大写字母的ASCII码加上32，就可以得到对应的小写字母的ASCII码。

# 相关资料：
- C语言参考手册：https://en.cppreference.com/w/c
- 字符串处理函数：https://en.cppreference.com/w/c/string/byte
- ASCII码表：http://www.asciitable.com/