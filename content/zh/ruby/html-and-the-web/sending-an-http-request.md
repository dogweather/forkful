---
date: 2024-01-20 18:00:19.569910-07:00
description: "How to: (\u600E\u4E48\u505A) \u5728Ruby\u4E2D\uFF0C\u53D1\u9001HTTP\u8BF7\
  \u6C42\u53EF\u4EE5\u7528\u51E0\u4E2A\u4E0D\u540C\u7684\u5E93\u3002\u8FD9\u91CC\u6211\
  \u4EEC\u7528`net/http`\uFF0C\u8FD9\u662FRuby\u6807\u51C6\u5E93\u7684\u4E00\u90E8\
  \u5206\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:48.370186-06:00'
model: gpt-4-1106-preview
summary: "\u5728Ruby\u4E2D\uFF0C\u53D1\u9001HTTP\u8BF7\u6C42\u53EF\u4EE5\u7528\u51E0\
  \u4E2A\u4E0D\u540C\u7684\u5E93\u3002\u8FD9\u91CC\u6211\u4EEC\u7528`net/http`\uFF0C\
  \u8FD9\u662FRuby\u6807\u51C6\u5E93\u7684\u4E00\u90E8\u5206."
title: "\u53D1\u51FA HTTP \u8BF7\u6C42"
weight: 44
---

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
