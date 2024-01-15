---
title:                "解析HTML"
html_title:           "C: 解析HTML"
simple_title:         "解析HTML"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/parsing-html.md"
---

{{< edit_this_page >}}

## 为什么

如果你是一位Web开发人员，那么你肯定知道HTML是构建网站的基本语言。但是，在大多数情况下，网站上的HTML代码并不完美。面对复杂的HTML结构，我们需要一个方法来从中提取出我们需要的信息。这就是为什么解析HTML是重要的，它可以让我们更轻松地从网页中提取出所需的数据。

## 如何解析HTML

```C
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main() {
    // 我们要解析的HTML字符串
    char html[] = "<h1>欢迎来到我的网站！</h1>";

    // 创建一个指向HTML字符串的指针
    char *p;

    // 使用strtok函数按照特定的标签来切分字符串
    p = strtok(html, "<>");
    while (p != NULL) {
        // 如果字符串以"h1"开头，我们就找到了标题
        if (strncmp(p, "h1", 2) == 0) {
            // 使用strtok再次切分字符串，这次按照">"来切分
            p = strtok(NULL, ">");
            printf("%s\n", p);
        }
        p = strtok(NULL, "<>");
    }
    return 0;
}
```

输出结果为：

```
欢迎来到我的网站！
```

上面的代码展示了如何使用C语言中的标准库函数来解析HTML字符串。首先，我们使用strtok函数按照特定的标签来切分字符串，然后再利用基于字符串比较的方法来获取所需的数据。通过这种简单的方法，我们就能够从HTML代码中提取出我们需要的信息。

## 深入解析HTML

如果你想要更深入地了解如何解析HTML，你可以研究一下HTML文档树（Document Object Model，简称DOM）。DOM是一种用来表示文档结构的对象模型，它可以让我们更方便地获取并操作HTML文档中的数据。C语言中也有一些库可以帮助我们操作DOM，比如libxml2和expat。

## 更多学习资源

- [使用C语言解析HTML（美文译文）](https://www.ibm.com/developerworks/cn/xml/x-doxygen/comment/#Mr5)
- [C语言解析HTML（英文原文）](https://dmitryfrank.com/articles/parsing_html_with_c)
- [libxml2官方文档](http://xmlsoft.org/html/index.html)
- [expat官方文档](https://libexpat.github.io/)

## 参考资料

- [如何使用C语言解析HTML（知乎问答）](https://www.zhihu.com/question/24380523/answer/54410926)
- [使用C语言处理HTML标签（博客文章）](https://blog.csdn.net/renfufei/article/details/78543135)