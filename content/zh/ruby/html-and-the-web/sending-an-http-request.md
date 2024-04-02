---
date: 2024-01-20 18:00:19.569910-07:00
description: "\u53D1\u9001HTTP\u8BF7\u6C42\u5C31\u662F\u8BA9\u4F60\u7684\u4EE3\u7801\
  \u548C\u7F51\u4E0A\u7684\u670D\u52A1\u5668\"\u804A\u5929\"\u3002\u7A0B\u5E8F\u5458\
  \u8FD9\u4E48\u505A\u662F\u56E0\u4E3A\u4ED6\u4EEC\u9700\u8981\u4ECE\u4E92\u8054\u7F51\
  \u83B7\u53D6\u6216\u53D1\u9001\u6570\u636E\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:48.370186-06:00'
model: gpt-4-1106-preview
summary: "\u53D1\u9001HTTP\u8BF7\u6C42\u5C31\u662F\u8BA9\u4F60\u7684\u4EE3\u7801\u548C\
  \u7F51\u4E0A\u7684\u670D\u52A1\u5668\"\u804A\u5929\"\u3002\u7A0B\u5E8F\u5458\u8FD9\
  \u4E48\u505A\u662F\u56E0\u4E3A\u4ED6\u4EEC\u9700\u8981\u4ECE\u4E92\u8054\u7F51\u83B7\
  \u53D6\u6216\u53D1\u9001\u6570\u636E\u3002"
title: "\u53D1\u51FA HTTP \u8BF7\u6C42"
weight: 44
---

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
