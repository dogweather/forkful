---
title:                "Ruby: 发送http请求"
simple_title:         "发送http请求"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 为什么

当您在开发网站或应用程序时，您经常需要从其他服务器获取数据或资源。发送HTTP请求是一种常用的方式来实现这一目的。它允许您向服务器发送请求，然后接收并处理返回的数据。因此，学习如何发送HTTP请求是很重要的，它能让您的应用程序具有更多的功能和灵活性。

## 如何做

首先，您需要安装Ruby的HTTP请求库，例如 `net/http`。然后，您可以定义一个方法来发送HTTP请求并处理返回的数据。以下是发送GET请求的示例代码：

```Ruby
require 'net/http'

url = URI.parse('https://www.example.com') # 将URL解析为URI对象
response = Net::HTTP.get_response(url) # 发送GET请求并将响应保存在response变量中
puts response.body # 输出响应的正文内容
```

该代码将向 `https://www.example.com` 发送一个GET请求，并输出返回的HTML文件。您也可以通过设置请求头信息和请求体来发送更复杂的HTTP请求。例如，以下代码将向服务器发送一个带有自定义请求头信息和请求体的POST请求：

```Ruby
require 'net/http'

url = URI.parse('https://www.example.com')
request = Net::HTTP::Post.new(url) # 创建POST请求对象
request.body = "name=John&age=25" # 设置请求体
request['Accept-Language'] = 'en-US' # 设置请求头信息
response = Net::HTTP.start(url.host, url.port) do |http|
  http.request(request) # 发送POST请求，并将响应保存在response变量中
end
puts response.body # 输出响应的正文内容
```

## 深入了解

您可以使用 `Net::HTTP` 库提供的各种方法和选项来发送各种类型的HTTP请求，并处理不同类型的响应。例如，您可以使用 `response.code` 来获取响应的状态码，或者使用 `response.body` 来获取响应的正文内容。您还可以使用 `request['Header-Name']` 来设置请求头信息，或使用 `request.body=` 来设置请求体。您可以根据自己的需求来灵活使用这些方法和选项，以实现各种各样的功能。

## 参考链接

- [Ruby官方文档](https://ruby-doc.org/stdlib-2.7.0/libdoc/net/http/index.html)
- [Ruby HTTP请求教程](https://www.digitalocean.com/community/tutorials/how-to-use-net-http-to-send-http-requests-in-ruby)
- [Net::HTTP库介绍](https://robertsahlin.com/make-http-requests-ruby-net-http-library/)
- [HTTP状态码参考](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status)
- [HTTP请求和响应概述](https://www.tutorialspoint.com/http/index.htm)

## 参见

- [Ruby文档 - Net::HTTP](https://ruby-doc.org/stdlib-2.7.0/libdoc/net/http/index.html)
- [Ruby on Rails文档 - HTTP客户端库](https://guides.rubyonrails.org/v6.1.4.1/http_client.html)