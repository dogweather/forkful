---
title:                "使用基本认证发送http请求"
html_title:           "Bash: 使用基本认证发送http请求"
simple_title:         "使用基本认证发送http请求"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 什么和为什么？
通过基本身份验证发送HTTP请求就是在网络通信数据时，进行权限验证的一种方式。程序员之所以这样做，主要是为了保护数据的安全性，只有拥有正确验证信息的用户才能访问数据。

## 如何操作：
```Arduino
#include <ESP8266WiFi.h>
#include <ESP8266HTTPClient.h>
 
const char* ssid = "你的WiFi名";
const char* password =  "你的WiFi密码";
 
void setup() {
 
  Serial.begin(115200);
  WiFi.begin(ssid, password);
 
  while (WiFi.status() != WL_CONNECTED) {
 
    delay(1000);
    Serial.print("连接中…");
 
  }
 
}

void loop() {
 
  if (WiFi.status() == WL_CONNECTED) { //只有在连接到WiFi后才发送HTTP请求
 
    HTTPClient http;  
 
    http.begin("http://你的域名");                                 
    http.addHeader("Authorization", "Basic 你的验证信息"); //添加基本身份验证头
 
    int httpCode = http.GET();                                       
 
    if (httpCode > 0) { 

      String payload = http.getString();
      Serial.println(payload);
      
    }
  
  }
 
  delay(30000); //在每个请求之间暂停30秒
 
}
```
## 深入理解：
基本身份验证是一种由HTTP协议定义的早期验证方式，然后在互联网上广泛使用。尽管其在安全性上存在一些弱点，如易受密码破解的攻击，但其易用性和广泛浏览器支持使之持续流行。然而，现代的框架和语言，如Python、Node.js等，都有其他更安全的身份验证方式，如Token验证和OAuth。具体使用哪种验证方式取决于你的需求和产品的目标。

## 查看更多：
你可以在以下网站上查看更多相关信息：
- Arduino官方文档：[http://arduino.cc/en/Main/Documentation](http://arduino.cc/en/Main/Documentation)
- ESP8266模块和WiFi模块：[http://esp8266.net/](http://esp8266.net/)
- HTTP基本身份验证：[https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Authentication](https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Authentication)