---
title:                "使用基本认证发送http请求"
html_title:           "Bash: 使用基本认证发送http请求"
simple_title:         "使用基本认证发送http请求"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 什么和为什么？
HTTP请求的基本认证即是一个验证用户身份的简单过程，通过发送包含用户名和密码细节的HTTP请求实现。程序员使用它主要是为了保护重要的信息，防止未经授权的访问。

## 如何操作：
Ruby使得发送带有基本认证的HTTP请求变得很容易。 下面是一个例子：

```Ruby
require 'net/http'
require 'uri'
uri = URI('http://example.com/data')
req = Net::HTTP::Get.new(uri)
req.basic_auth 'user', 'pass'
res = Net::HTTP.start(uri.hostname, uri.port) {|http|
  http.request(req)
}
```

运行上述代码，你将看到一个典型的HTTP响应。本例中，我们将'example.com'的数据页面赋值给uri,并设置了一个带有基本认证的GET请求。

## 深入探讨：
基本认证（Basic Authentication）在互联网的早期阶段就开始使用。然而，由于它存在一些明显的安全问题（例如密码以明文传输），因此并不总是首选的认证方式。

另外一种常见替代方法是摘要认证（Digest Authentication），它提供了一些附加的安全措施。然而这需要更复杂的实现，并且在Ruby中，使用`net/http`库的方式并不直观。

至于操作细节，Ruby的`Net::HTTP::Get`类提供了一个名为`basic_auth`的方法，它将用户名和密码转化为一个适当的`Authorization`头部（header）。此后，包含这个头部的HTTP请求被发送出去。

## 参阅：
1. [Ruby Doc: Net::HTTP](https://ruby-doc.org/stdlib-2.6.3/libdoc/net/http/rdoc/Net/HTTP.html)
2. [Wikipedia: Basic access authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)
3. [RFC2617: HTTP Authentication](https://tools.ietf.org/html/rfc2617)

由于我们没有'总结'章节，此处就到此为止。这个指南应该对你在Ruby中发送带有基本认证的HTTP请求有所帮助。