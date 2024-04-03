---
date: 2024-01-20 17:44:40.234280-07:00
description: "How to (\u5982\u4F55\u64CD\u4F5C) \u5728Ruby\u91CC\uFF0C\u4F60\u53EF\
  \u4EE5\u4F7F\u7528`net/http`\u5E93\u7B80\u5355\u5FEB\u901F\u5730\u4E0B\u8F7D\u7F51\
  \u9875\u3002\u4EE5\u4E0B\u662F\u4E2A\u793A\u4F8B\uFF1A."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:48.372166-06:00'
model: gpt-4-1106-preview
summary: "\u5728Ruby\u91CC\uFF0C\u4F60\u53EF\u4EE5\u4F7F\u7528`net/http`\u5E93\u7B80\
  \u5355\u5FEB\u901F\u5730\u4E0B\u8F7D\u7F51\u9875\u3002\u4EE5\u4E0B\u662F\u4E2A\u793A\
  \u4F8B\uFF1A."
title: "\u4E0B\u8F7D\u7F51\u9875"
weight: 42
---

## How to (如何操作)
在Ruby里，你可以使用`net/http`库简单快速地下载网页。以下是个示例：

```Ruby
require 'net/http'
require 'uri'

uri = URI('http://www.example.com')
response = Net::HTTP.get(uri)

puts response
```

如果一切顺利，你会看到终端打印出`example.com`的HTML代码。

## Deep Dive (深入了解)
在Ruby早期版本，下载网页可能需用`open-uri`或外部的gem，比如`rest-client`。`net/http`是Ruby自带库，因此你不需要安装额外的gem。

另外，你还可以处理重定向、设置请求头或使用HTTPS协议。处理复杂情况时，更全面的库如`Faraday`或`HTTParty`会是不错的选择。

使用`net/http`时，请注意可能的异常（比如网络问题导致的超时）。

## See Also (另请参阅)
- Ruby官方文档 [`Net::HTTP`](https://ruby-doc.org/stdlib-3.0.0/libdoc/net/http/rdoc/Net/HTTP.html)
- [`HTTParty`](https://github.com/jnunemaker/httparty)
- [`Faraday`](https://github.com/lostisland/faraday)
