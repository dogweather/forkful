---
title:                "插值字符串"
html_title:           "Arduino: 插值字符串"
simple_title:         "插值字符串"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 什么以及为什么？
字符串插值是把变量值放入字符串的过程。程序员使用它以便构建和操作可读性高且内容动态改变的字符串。

## 如何去做：
以下是一个进行字符串插值的C编程示例和输出。

```C 
#include <stdio.h>

int main() {
    char name[] = "Tom";
    int age = 25;

    printf("Hello, my name is %s. I am %d years old.\n", name, age);

    return 0;
}
```
执行这段代码的输出将是：
``` 
Hello, my name is Tom. I am 25 years old.
``` 

## 深度研究
1. 历史背景: 字符串插值在古老的编程语言PRINT语句中已经存在。C语言中使用printf函数和格式说明符实现它。
2. 可选方案：C++11之后增加了std::to_string方法去转换数值为字符串形式。然后你可以用+运算符连接它们。
3. 实现细节：C语言中的printf函数把格式说明符（比如%s或%d）替换为提供的变量值。 它做这个是通过“stdarg”库和特定的参数类型。

## 参阅：
1. C语言Printf函数详解: https://goo.gl/mY2Jv8
2. C语言字符和字符串: https://goo.gl/W4VSy8
3. C语言资料库：https://www.runoob.com/cprogramming/c-tutorial.html