---
title:                "连接字符串"
html_title:           "C: 连接字符串"
simple_title:         "连接字符串"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
在C语言编程中，字符串连接是把两个或更多的字符串连接到一起形成一个新的字符串。程序员之所以要进行字符串连接，主要是为了方便对字符串的处理和操作，比如向用户显示信息、构造SQL查询或者构造文件路径等。

## 如何操作：
为了展示如何在C语言中进行字符串连接，让我们来看看下面的例子：

```C
#include <string.h>
#include <stdio.h>

int main()
{
    char str1[100] = "Hello, ";
    char str2[] = "World!";
    strcat(str1, str2);
    printf("%s\n", str1);
    return 0;
}
```

运行这段代码，输出结果会是：

```C
Hello, World!
```

## 深度剖析:
字符串连接在C语言的历史中始终有着重要的地位。它的存在让字符串的处理变得简单明了。虽然有许多其它的方法可以完成字符串的连接，但C语言中使用strcat函数是最常用的一种。strcat函数将会把源字符串添加到目标字符串的结尾，因此，必须确保目标字符串有足够的空间以避免溢出。

当然，也有其它的方法来连接字符串，比如snprintf函数，这个函数可以设置目标字符串的最大长度，避免了字符串溢出的可能。

## 另请参见：
3. 维基百科上关于字符串连接的更多信息：[String concatenation](https://en.wikipedia.org/wiki/String_concatenation)