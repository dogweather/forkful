---
title:                "使用基本身份验证发送http请求"
html_title:           "Arduino: 使用基本身份验证发送http请求"
simple_title:         "使用基本身份验证发送http请求"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 为什么
使用基本认证发送HTTP请求的原因通常是为了访问需要用户验证的网站或API。基本认证是一种简单的身份验证方法，可以保护您的个人信息和数据安全。

## 如何操作
### 准备工作
要使用Arduino发送HTTP请求，您需要准备以下物品：
- Arduino主板
- 一块Wi-Fi模块（如ESP8266或ESP32）
- USB数据线
- 访问需要身份验证的网站或API的URL

### 开始编码
首先，将Wi-Fi模块连接到Arduino主板。然后，在Arduino IDE中打开新的项目。接下来，使用以下代码框来表示Arduino和Wi-Fi模块的库和变量。
```Arduino
// 引入所需的库
#include <ESP8266WiFi.h>
 
// Wi-Fi模块的静态IP地址
IPAddress ip(192,168,1,102);

// Wi-Fi模块的SSID和密码
const char* ssid = "您的Wi-Fi名称";
const char* password = "您的Wi-Fi密码";
 
void setup() {
  // 初始化Wi-Fi连接
  WiFi.begin(ssid, password);
  // 将Wi-Fi模块连接设置为静态IP地址
  WiFi.config(ip);
  // 等待Wi-Fi连接完成
  while (WiFi.status() != WL_CONNECTED) {
    delay(1000);
    Serial.println("正在连接至Wi-Fi...");
  }
}

void loop() {
  // 在这里输入发送HTTP请求的代码
}
```

接下来，我们将编写代码来发送HTTP请求。首先，定义需要访问的URL和HTTP头部信息（包括基本认证的用户名和密码）。
```Arduino
String url = "您的URL";
String auth = "Basic 用户名:密码";
```
然后，使用以下代码来发送HTTP请求并获取响应。
```Arduino
HTTPClient http;
http.begin(url); // 指定要访问的URL
http.addHeader("Authorization", auth); // 添加HTTP头部信息
int httpResponseCode = http.GET(); // 发送HTTP GET请求并获取响应代码
String response = http.getString(); // 获取响应内容
http.end(); // 结束连接
```
最后，在串行监视器中打印响应代码和响应内容，可以看到HTTP请求是否成功以及返回的数据。
```Arduino
Serial.println(httpResponseCode);
Serial.println(response);
```

## 深入了解
如果您想深入了解发送HTTP请求的原理，可以阅读关于HTTP协议和基本认证的相关文章。基本的HTTP请求过程包括与服务器建立连接、发送请求、接收响应等步骤，而基本认证则是在请求中包含用户名和密码的方式来进行身份验证。

## 另请参阅
- [HTTP协议简介](https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Overview)
- [基本认证指南](https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Authentication#Basic_authentication_scheme)