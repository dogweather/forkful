---
date: 2024-01-20 17:44:04.098607-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) \u4E0B\u8F7D\u7F51\u9875\u8FD9\
  \u4E2A\u4EFB\u52A1\u53EF\u4EE5\u8FFD\u6EAF\u5230\u7F51\u7EDC\u5F00\u59CB\u6D41\u884C\
  \u7684\u5E74\u4EE3\u3002\u65E9\u671F\u7684\u4E92\u8054\u7F51\u63A2\u7D22\u8005\u4F7F\
  \u7528telnet\u6216\u8005FTP\uFF0C\u4F46\u8FD9\u4E9B\u65B9\u6CD5\u9010\u6E10\u88AB\
  HTTP\u534F\u8BAE\u53D6\u4EE3\u3002\u73B0\u5728\uFF0C\u6211\u4EEC\u6709\u4E86\u4E13\
  \u95E8\u7684\u547D\u4EE4\u884C\u5DE5\u5177\u6765\u5904\u7406\u8FD9\u9879\u5DE5\u4F5C\
  \uFF0C\u6BD4\u5982`curl`\u548C`wget`\u3002`curl`\u662FFish\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.539583-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C\uFF1A) \u4E0B\u8F7D\u7F51\u9875\u8FD9\u4E2A\u4EFB\
  \u52A1\u53EF\u4EE5\u8FFD\u6EAF\u5230\u7F51\u7EDC\u5F00\u59CB\u6D41\u884C\u7684\u5E74\
  \u4EE3\u3002\u65E9\u671F\u7684\u4E92\u8054\u7F51\u63A2\u7D22\u8005\u4F7F\u7528telnet\u6216\
  \u8005FTP\uFF0C\u4F46\u8FD9\u4E9B\u65B9\u6CD5\u9010\u6E10\u88ABHTTP\u534F\u8BAE\u53D6\
  \u4EE3\u3002\u73B0\u5728\uFF0C\u6211\u4EEC\u6709\u4E86\u4E13\u95E8\u7684\u547D\u4EE4\
  \u884C\u5DE5\u5177\u6765\u5904\u7406\u8FD9\u9879\u5DE5\u4F5C\uFF0C\u6BD4\u5982`curl`\u548C\
  `wget`\u3002`curl`\u662FFish Shell\u4E2D\u5E38\u7528\u7684\u5DE5\u5177\u4E4B\u4E00\
  \uFF0C\u529F\u80FD\u5F3A\u5927\u4E14\u4F7F\u7528\u7B80\u5355\u3002\u5B83\u53EF\u4EE5\
  \u5904\u7406\u5404\u79CDWeb\u534F\u8BAE\uFF0C\u652F\u6301\u4EE3\u7406\uFF0C\u8BA4\
  \u8BC1\uFF0C\u8FD8\u6709\u63D0\u4EA4\u6570\u636E\u7B49\u64CD\u4F5C\u3002\u800C`wget`\u662F\
  \u53E6\u4E00\u4E2A\u6D41\u884C\u7684\u9009\u62E9\uFF0C\u7279\u522B\u662F\u5B83\u7684\
  \u9012\u5F52\u4E0B\u8F7D\u529F\u80FD\u8BA9\u6574\u7AD9\u4E0B\u8F7D\u53D8\u5F97\u7B80\
  \u5355\u3002\u4E0D\u8FC7\uFF0C\u5728Fish Shell\u4E2D\u4F7F\u7528\u8FD9\u4E9B\u5DE5\
  \u5177\u90FD\u975E\u5E38\u76F4\u63A5\u3002"
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
