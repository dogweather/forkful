---
title:                "解析HTML"
html_title:           "Bash: 解析HTML"
simple_title:         "解析HTML"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/parsing-html.md"
---

{{< edit_this_page >}}

## 为什么

在今天的数字化世界中，网页和网站无处不在。操作HTML的能力可以让你更加灵活地提取有用的信息，从而让你的工作效率更高。

## 怎么做

要在Bash中解析HTML，我们需要使用工具**curl**和**grep**。首先，使用curl从网页中下载HTML代码，并将其存储到一个变量中，例如：

```Bash
html_code=$(curl www.example.com)
```

接下来，我们可以使用grep命令来定位我们所需的信息。例如，假设我们想要提取所有的<strong>标签中的文本，我们可以使用该命令：

```Bash
echo "$html_code" | grep -oE "<strong>.*</strong>"
```

这将会输出所有的<strong>标签中的文本，以供我们进一步处理。这只是一个简单的例子，你可以根据需要使用不同的grep模式来提取你想要的信息。

## 深入探究

解析HTML需要一定的技巧和经验。一个值得注意的技巧是使用"gawk"来更容易地处理HTML代码。此外，理解HTML的结构和标签之间的关系也是至关重要的。你可以通过阅读W3School的教程来学习更多关于HTML的知识。

## 参考链接

- [curl man page](https://linux.die.net/man/1/curl)
- [grep man page](https://linux.die.net/man/1/grep)
- [Linux Journey：使用Bash解析HTML](https://linuxjourney.com/lesson/parsing-html-bash)