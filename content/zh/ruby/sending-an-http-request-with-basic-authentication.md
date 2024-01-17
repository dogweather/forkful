---
title:                "你喜欢编程吗？使用基本认证发送http请求"
html_title:           "Ruby: 你喜欢编程吗？使用基本认证发送http请求"
simple_title:         "你喜欢编程吗？使用基本认证发送http请求"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 什麼 & 為甚麼？

發送具有基本驗證的HTTP請求是一種常見的網絡通信方式，它使得程序員可以在發送請求時驗證自己的身份。這樣可以確保只有授權的用戶可以訪問受保護的資源，從而增強了網絡安全性。

## 如何實現：

```ruby
# 首先需要引入 "net/http" 和 "uri" 模塊
require 'net/http'
require 'uri'
 
# 通過 URI 對象定義請求地址
http_uri = URI.parse('https://example.com/api')

# 設置需要驗證的用戶名和密碼
username = 'username'
password = 'password'

# 使用 "Net::HTTP::Get" 方法發送帶有驗證的請求
request = Net::HTTP::Get.new(http_uri, { 'Authorization' => "Basic #{Base64.encode64("#{username}:#{password}")}" })

# 使用 "Net::HTTP" 模塊發送請求
response = Net::HTTP.start(http_uri.hostname, http_uri.port, use_ssl: http_uri.scheme == 'https') do |http|
  http.request(request)
end

# 打印服務器返回的回復狀態碼和回復主體
p "Status code: #{response.code}"
p "Response body: #{response.body}"
```

## 深入了解：

發送具有基本驗證的HTTP請求可以追溯到早期的網絡通信方式，它基於HTTP的身份驗證標準。對於程序員來說，還有其他替代方式可供選擇，例如使用OAuth 2.0協議進行驗證。 在實際實現時，還需注意在發送請求時使用加密的HTTPS協議來保護用戶數據的安全性。

## 參考鏈接：

- [Ruby Net::HTTP文檔](https://ruby-doc.org/stdlib-2.7.2/libdoc/net/http/rdoc/Net/HTTP.html)
- [URI模塊文檔](https://ruby-doc.org/stdlib-2.7.2/libdoc/uri/rdoc/URI.html)
- [HTTP基本認證標準](https://tools.ietf.org/html/rfc7617)