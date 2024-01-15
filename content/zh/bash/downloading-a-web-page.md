---
title:                "下载一个网页"
html_title:           "Bash: 下载一个网页"
simple_title:         "下载一个网页"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 为什么

下载网页可能是一个很有用的技能，尤其是当你想要保存网页的内容，或者离线浏览网页时。使用Bash编程可以很方便地实现这个功能。

## 如何操作

首先，打开终端并导航到你想要保存网页的位置。然后，使用下面的命令来下载网页：

```Bash
wget [网页链接]
```

示例：如果你想要下载维基百科的首页，可以使用这个命令：

```Bash
wget https://zh.wikipedia.org/wiki/Wikipedia:%E9%A6%96%E9%A1%B5
```

下载完成后，你可以在当前目录下找到一个名为“Wikipedia:首页”的文件，它就是你下载的网页。

## 深入了解

通过命令行工具下载网页使用的是HTTP协议。这个协议定义了Web服务器和客户端之间传输数据的标准。使用wget命令，我们可以很容易地访问远程服务器，获取服务器传输的数据，并将其保存为一个文件。wget还可以设置用户代理、限制下载速度和处理重定向等功能。

## 参考链接

- [wget使用指南](https://www.gnu.org/software/wget/manual/wget.html)
- [Bash编程基础](https://wangdoc.com/bash/intro.html)
- [HTTP协议简介](https://www.runoob.com/http/http-tutorial.html)

## 参见