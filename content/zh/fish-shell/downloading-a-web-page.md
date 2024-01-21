---
title:                "下载网页"
date:                  2024-01-20T17:44:04.098607-07:00
model:                 gpt-4-1106-preview
simple_title:         "下载网页"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么？)

下载网页意味着从互联网上抓取一整个页面的数据。程序员这么做的目的通常是为了数据分析，内容备份，或者是离线查看。

## How to: (如何操作：)

```Fish Shell
# 使用curl命令下载页面
curl https://example.com -o saved_page.html

# 输出样例查看文件是否下载成功
cat saved_page.html
```

## Deep Dive (深入探索)

下载网页这个任务可以追溯到网络开始流行的年代。早期的互联网探索者使用telnet或者FTP，但这些方法逐渐被HTTP协议取代。现在，我们有了专门的命令行工具来处理这项工作，比如`curl`和`wget`。`curl`是Fish Shell中常用的工具之一，功能强大且使用简单。它可以处理各种Web协议，支持代理，认证，还有提交数据等操作。而`wget`是另一个流行的选择，特别是它的递归下载功能让整站下载变得简单。不过，在Fish Shell中使用这些工具都非常直接。

## See Also (另请参阅)

- curl官网文档: [https://curl.se/docs/manual.html](https://curl.se/docs/manual.html)
- wget官网文档: [https://www.gnu.org/software/wget/manual/wget.html](https://www.gnu.org/software/wget/manual/wget.html)
- Fish Shell官方文档: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)