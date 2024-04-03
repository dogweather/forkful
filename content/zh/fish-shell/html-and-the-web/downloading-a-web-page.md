---
date: 2024-01-20 17:44:04.098607-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:48.264279-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u4E0B\u8F7D\u7F51\u9875"
weight: 42
---

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
