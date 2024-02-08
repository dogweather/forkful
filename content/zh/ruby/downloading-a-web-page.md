---
title:                "下载网页"
aliases:
- zh/ruby/downloading-a-web-page.md
date:                  2024-01-20T17:44:40.234280-07:00
model:                 gpt-4-1106-preview
simple_title:         "下载网页"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
下载网页就是把网页内容从网络上拿到你的电脑里。程序员这么做可能是为了数据分析，或者是为了网页备份。

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
