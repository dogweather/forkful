---
title:                "下载网页"
html_title:           "Bash: 下载网页"
simple_title:         "下载网页"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 什么是网页下载? 
网页下载是指通过计算机程序从互联网上下载网页的过程。程序员经常会下载网页来获取网络上的信息，以便于分析、处理和展示。

## 如何实现: 
在Bash中，你可以使用curl命令来下载网页。它以命令行的形式接受一个网页的URL作为输入，并将其内容保存到本地文件中。以下是一个示例代码和输出：

```Bash 
curl https://example.com > output.html 
```

假设我们要下载[百度首页](https://www.baidu.com)，我们可以这样写：

```Bash
curl https://www.baidu.com > baidu.html
```

这将会在当前目录下创建一个名为"baidu.html"的文件，其中包含着百度首页的内容。

## 深入了解: 
网页下载作为一项技术已经存在了很久。在早期的互联网时代，人们使用FTP（文件传输协议）来下载网页。但随着技术的发展，出现了更加方便和实用的下载工具，如HTTP（超文本传输协议）和curl命令。除了curl命令，还有其他一些工具也可以在Bash中实现网页下载，如wget命令和Python的urllib模块。

## 参考资料: 
- [curl命令文档](https://curl.haxx.se/docs/) 
- [wget命令文档](https://www.gnu.org/software/wget/manual/wget.html) 
- [urllib模块文档](https://docs.python.org/3/library/urllib.html)