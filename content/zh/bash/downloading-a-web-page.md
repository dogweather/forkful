---
title:                "下载网页"
html_title:           "Arduino: 下载网页"
simple_title:         "下载网页"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 什么和为什么？
访问和下载网页是在互联网中获取具体网页内容的过程。程序员通常这样做以获取、分析和利用网页数据。

## 如何做：
Bash中最基础的命令之一是curl。这是一个简单的使用curl下载网页的例子：
```Bash
curl https://www.example.com -O index.html
```
这将会下载名为'index.html'的文件到你的当前目录下。注意，使用-O参数时，这将保留服务器提供的文件名。如果你想要自己命名，可以使用-o参数，如：

```Bash
curl https://www.example.com -o my_page.html
```
这样就会在你的当前目录下载一个名为'my_page.html'的文件。

## 深入理解：
在互联网的早期，下载网页主要是浏览器的职责。但是，随着时间的推移，各种编程语言和工具，如Python、Ruby、Bash等，开始为此功能提供内置的库和命令。

Bash通过curl和wget提供了下载网页的功能。curl是一个轻量级的命令行工具，主要用于下载和上传数据。wget则是一个更强大的工具，可以用于下载整个网站或者镜像。

这两种工具基本上使用了相同的方法：通过HTTP或HTTPS请求来获取服务器的响应，然后将响应内容保存到文件中。

在某些情况下，你可能遇到需要登陆才能访问的页面，curl和wget都提供了处理这种情况的方法。你可以在你的命令中提供用户名和密码，或使用存储在文件中的cookie。

## 另外参阅：
- [在Bash中使用curl的官方资源](https://curl.se/docs/manpage.html)
- [在Bash中使用wget的官方资源](https://www.gnu.org/software/wget/manual/wget.html)
- [Bash编程的相关教程](https://www.tutorialspoint.com/unix/unix-using-variables.htm)
- [关于HTTP/HTTPS请求的相关信息](https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview)