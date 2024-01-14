---
title:                "Ruby: 用基本身份验证发送http请求"
simple_title:         "用基本身份验证发送http请求"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

本文將介紹如何使用Ruby編程語言發送HTTP請求，並使用基本身份驗證進行身份驗證。HTTP請求和身份驗證是網絡應用程序開發中不可或缺的部分，理解如何使用它們對於開發者來說非常重要。如果您對如何發送帶基本身份驗證的HTTP請求感到困惑，讀完本文後，您將對此有更深入的理解。

## 為何

發送HTTP請求通常用於向遠程服務器發送請求，以獲取數據或執行某些操作。當遠程服務器需要用戶的身份驗證信息時，就需要使用基本身份驗證方式。

## 如何處理

現在我們使用Ruby的Net::HTTP庫來發送帶基本身份驗證的HTTP請求。首先，我們需要按照以下步驟進行配置：

1. 引入`net/http`庫。
2. 創建一個`Net::HTTP`對象，並指定要訪問的URL。
3. 設置`Net::HTTP`對象的`use_ssl`屬性為`true`，以達到使用HTTPS的效果。
4. 設置`Net::HTTP`對象的`basic_auth`方法，並傳入用戶名和密碼作為參數。

編碼示例：

```Ruby
require 'net/http'
uri = URI("https://example.com") # 假設要發送請求的網址為example.com
http = Net::HTTP.new(uri.host, uri.port)
http.use_ssl = true # 如果使用HTTPS則需要設置為true
http.basic_auth("username", "password") # 填寫您的用戶名和密碼
```

如果您需要傳遞其他參數，可以使用`Net::HTTP`對象的`post`或`get`方法。

```Ruby
# 藉由GET方法獲取數據
request = Net::HTTP::Get.new(uri.request_uri)
response = http.request(request)

# 藉由POST方法傳遞數據
request = Net::HTTP::Post.new(uri.request_uri)
request.set_form_data({"key" => "value"}) # 改為您要傳遞的參數
response = http.request(request)
```

## 深入挖掘

基本身份驗證通過在HTTP請求中添加`Authorization`頭信息來進行身份驗證。這個頭信息包含用戶名和密碼的Base64編碼。Base64編碼是一種對二進制數據進行編碼的方法，可以讓二進制數據以ASCII可讀的形式表示。請注意，Base64編碼並不是真正的加密，只是將數據進行編碼以方便在網絡中傳輸。

現在，您應該已經了解如何使用Ruby發送帶基本身份驗證的HTTP請求了。但是，如果您需要更深入了解身份驗證的過程，可以參考以下文章：

- [HTTP 基本認證](https://developer.mozilla.org/zh-TW/docs/Web/HTTP/Authentication)
- [HTTP 基本認證攻擊](https://www.imperva.com/learn/application-security/basic-authentication/)
- [