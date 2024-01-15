---
title:                "发送一个http请求。"
html_title:           "Ruby: 发送一个http请求。"
simple_title:         "发送一个http请求。"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

# 为什么要发送HTTP请求？

在Web开发中，发送HTTP请求是非常常见的操作。它可以让你的程序与其他服务器进行通信，从而获取需要的数据或者执行特定的操作。

## 如何发送HTTP请求？

### 使用`Net::HTTP`库

Ruby提供了内置的`Net::HTTP`库来处理HTTP请求。你可以使用它来发送GET、POST、PUT等不同类型的请求。下面是一个简单的示例：

```Ruby
require 'net/http'

# 构建一个URI对象
uri = URI('https://www.example.com/users')

# 利用Net::HTTP发送GET请求并获取响应
response = Net::HTTP.get(uri)

# 打印响应的内容
puts response
```

这里我们利用`Net::HTTP`库构建一个URI对象，然后调用`get`方法发送GET请求，并将响应保存到`response`变量中。最后打印出响应的内容。

### 使用`RestClient`库

除了内置的`Net::HTTP`库，还有一个非常流行的HTTP客户端库是`RestClient`。它提供了更简洁的语法来发送HTTP请求。下面是一个使用`RestClient`库发送GET请求的示例：

```Ruby
require 'rest-client'

# 发送GET请求并获取响应
response = RestClient.get('https://www.example.com/users')

# 打印响应的内容
puts response.body
```

## 深入探讨HTTP请求

在发送HTTP请求时，我们通常需要设置一些请求头和请求体，以及处理响应的状态码和响应体。在使用`Net::HTTP`和`RestClient`库时，你可以通过相应的方法来设置或处理这些信息。

例如，对于`Net::HTTP`库，可以使用`set_form_data`方法来设置表单数据，使用`add_field`方法来添加请求头：

```Ruby
require 'net/http'

uri = URI('https://www.example.com/users')

# 设置表单数据
form_data = { name: 'John', email: 'john@example.com' }

# 构建一个POST请求
request = Net::HTTP::Post.new(uri)
request.set_form_data(form_data)
request.add_field('Authorization', 'Bearer 12345') # 添加请求头

# 发送请求并获取响应
response = Net::HTTP.start(uri.hostname, uri.port) do |http|
  http.request(request)
end

# 处理响应的状态码和响应体
if response.code == '200'
  puts "Successfully created user #{form_data[:name]}."
  puts response.body
else
  puts 'Oops! Something went wrong.'
end
```

而对于`RestClient`库，可以使用`get`、`post`等方法来发送不同类型的请求，并通过`response`对象来获取响应的信息：

```Ruby
require 'rest-client'

# 发送POST请求并获取响应
response = RestClient.post('https://www.example.com/users', { name: 'John', email: 'john@example.com' })

# 处理响应的状态码和响应体
if response.code == 201
  puts "Successfully created user #{body[:name]}."
  puts response.body
else
  puts 'Oops! Something went wrong.'
end
```

## 参考链接

- [Ruby 官方文档：Net::HTTP](https://ruby-doc.org/stdlib-2.7.1/libdoc/net/http/rdoc/Net/HTTP.html)
- [RubyGems：RestClient](https://rubygems.org/gems/rest-client/versions/1.9.0)
- [HTTP协议介绍](https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Overview)
- [HTTP状态码](https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Status)