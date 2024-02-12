---
title:                "发出 HTTP 请求"
aliases: - /zh/ruby/sending-an-http-request.md
date:                  2024-01-20T18:00:19.569910-07:00
model:                 gpt-4-1106-preview
simple_title:         "发出 HTTP 请求"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么?)
发送HTTP请求就是让你的代码和网上的服务器"聊天"。程序员这么做是因为他们需要从互联网获取或发送数据。

## How to: (怎么做)
在Ruby中，发送HTTP请求可以用几个不同的库。这里我们用`net/http`，这是Ruby标准库的一部分。

```Ruby
require 'net/http'
require 'uri'

uri = URI('http://example.com/some_path?query=string')
response = Net::HTTP.get_response(uri)

puts "Response code: #{response.code}"
puts "Headers: #{response.to_hash}"
puts "Body: #{response.body}"

# 示例输出：
# Response code: 200
# Headers: { "content-type": ["text/html; charset=UTF-8"], ... }
# Body: <!doctype html>...
```

## Deep Dive (深入了解)
### 历史背景
Ruby最早的HTTP库可能不够强大，但随着时间的推移，像`net/http`这样的库变得越来越稳定、灵活。

### 替代品
`net/http` 是内建的，但不一定是最好用的。流行的替代品有 `httparty` 和 `faraday`。

### 实现细节
`net/http` 里，`Net::HTTP.get_response(uri)` 是一个方便方法，一步获取响应。不过你可能需要手动管理连接、设置超时等。

## See Also (另请参阅)
- Ruby `net/http` 文档: [https://ruby-doc.org/stdlib-3.0.0/libdoc/net/http/rdoc/Net/HTTP.html](https://ruby-doc.org/stdlib-3.0.0/libdoc/net/http/rdoc/Net/HTTP.html)
- `httparty` gem: [https://github.com/jnunemaker/httparty](https://github.com/jnunemaker/httparty)
- `faraday` gem: [https://github.com/lostisland/faraday](https://github.com/lostisland/faraday)
