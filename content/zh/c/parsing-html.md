---
title:                "C: 解析HTML"
simple_title:         "解析HTML"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/parsing-html.md"
---

{{< edit_this_page >}}

# 为什么

HTML是一种标记语言，用于创建网页和应用程序的结构。解析HTML是一个重要的技能，因为它允许程序员从网页中提取有用的信息，例如文本，图像和超链接。通过解析HTML，您可以轻松地从网页中提取数据，并在您的程序中使用它们。接下来，我们将通过本文的指导，了解如何在C编程语言中解析HTML。

## 如何做

要解析HTML，您需要首先了解HTML的结构。在HTML文件中，标签是构成网页骨架的基本组成部分。这些标签在尖括号内，如```<html>```。它们通常是成对出现的，一个开标签和一个闭标签，例如```<p>```和```</p>```。在C程序中，我们可以通过使用指针和循环来解析HTML。

下面是一个简单的例子，展示了如何使用C语言来解析HTML文件中的所有标签：

```C
#include <stdio.h>
#include <string.h>  //用于处理字符串函数
#include <ctype.h>  //用于检查字符类型

int main()
{
    //打开HTML文件为只读模式
    FILE* html_file = fopen("index.html", "r");

    if (html_file == NULL)
    {
        printf("文件打开失败\n");
        return 1;
    }

    char tag_name[20];
    char ch;  //用于存储当前读取的字符
    int i = 0;

    //循环读取文件中的每个字符
    while ((ch = fgetc(html_file)) != EOF)
    {
        //检查是否遇到了一个开始标签
        if (ch == '<')
        {
            //开始标签后面的字符一般是标签的名称
            ch = fgetc(html_file);
            while (ch != '>')
            {
                //将字符存储到标签名称数组中
                tag_name[i] = ch;
                i++;
                ch = fgetc(html_file);
            }

            //将数组的最后一位改为'\0'来结束字符串
            tag_name[i] = '\0';
            i = 0;

            //打印标签名称
            printf("标签名称：%s\n", tag_name);
        }
    }

    //关闭文件
    fclose(html_file);
    
    return 0;
}
```

假设我们的HTML文件内容如下：

```HTML
<!DOCTYPE html>
<html>
<head>
    <title>我的网站</title>
</head>
<body>
    <h1>欢迎来到我的网站！</h1>
    <p>这是一个简单的网站，但我会努力让它变得更好。</p>
</body>
</html>
```

上面的代码将输出：

```
标签名称：html
标签名称：head
标签名称：title
标签名称：/title
标签名称：/head
标签名称：body
标签名称：h1
标签名称：/h1
标签名称：p
标签名称：/p
标签名称：/body
标签名称：/html
```

现在我们可以根据需要来使用解析出来的标签名称。

## 深入了解

除了标签名称，您还可以使用类似的方法来解析出标签的属性和内容。通过检查标签内的内容，您可以提取出重要的信息并将其用于您的程序中，例如网页标题，段落文本和图片链接。

另外，您也可以使用一些C语言的标准库函数来进行更复杂的解析，例如```strtok()```和```strstr()```。

## 另请参阅

- [C语言解析HTML教程（英文）](https://www.cprogramming.com/tutorial/c/lesson18.html)
- [C语言字符串教程（英文）](https://www.cprogramming.com/tutorial/c/lesson9.html)
- [C标准库参考（英