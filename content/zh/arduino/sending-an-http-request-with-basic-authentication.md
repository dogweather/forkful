---
title:                "使用基本认证发送一个HTTP请求。"
html_title:           "Arduino: 使用基本认证发送一个HTTP请求。"
simple_title:         "使用基本认证发送一个HTTP请求。"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# 什么是HTTP请求和基本认证？
发送HTTP请求是一种通过互联网向服务器请求数据的常见方式。基本认证是一种用于验证用户身份的简单方法，其中用户必须提供用户名和密码才能访问受保护的内容。程序员通常会使用HTTP请求和基本认证来与API交互，从而获取所需的数据或执行特定的功能。

## 如何实现：
```Arduino

#include <WiFiClientSecure.h>

// 设置WiFi网络名称和密码
const char* ssid = "YourWiFiNetwork";
const char* password =  "YourWiFiPassword";

// 设置服务器地址和端口
const char* server = "www.example.com";
int port = 443;

// 设置验证信息
const char* user = "Username";
const char* pass = "Password";

// 创建安全的WiFi客户端
WiFiClientSecure client;

void setup() {
  Serial.begin(9600);

  // 连接WiFi网络
  WiFi.begin(ssid, password);
  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.println("Connecting to WiFi..");
  }
  Serial.println("Connected to WiFi network!");

  // 配置客户端证书
  client.setCACert(cacertificate);

  // 发送HTTP请求
  client.setAuthorization(user, pass);
  client.println("GET /data HTTP/1.0");
  client.println("Host: www.example.com");
  client.println();

  // 读取响应
  while (client.connected()) {
    String line = client.readStringUntil('\n');
    Serial.println(line);
  }
  client.stop();
}

void loop() {
  // 程序循环
}
```

## 深入探讨：
HTTP请求和基本认证是Web开发中常用的技术。它们最早由Web的创建者提出，并被广泛接受为一种安全可靠的认证方法。除了基本认证，还有其他类型的认证方法，例如OAuth和Token认证。程序员通常会根据自己的需求选择最合适的认证方法。

## 参考资料：
- [HTTP请求基础知识](https://www.runoob.com/http/http-intro.html)
- [Arduino官方文档](https://www.arduino.cc/reference/en/)
- [基本认证的工作原理](https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Authentication)