---
aliases:
- /zh/arduino/sending-an-http-request-with-basic-authentication/
date: 2024-01-20 18:00:43.549718-07:00
description: "\u53D1\u9001\u5E26\u6709\u57FA\u672C\u8BA4\u8BC1\u7684HTTP\u8BF7\u6C42\
  \uFF0C\u5373\u7528\u7528\u6237\u540D\u548C\u5BC6\u7801\u901A\u8FC7\u7F51\u7EDC\u53D1\
  \u9001\u6570\u636E\u8BF7\u6C42\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\
  \u4E86\u5B89\u5168\u4EA4\u6362\u6570\u636E\uFF0C\u8BBF\u95EE\u53D7\u4FDD\u62A4\u7684\
  \u8D44\u6E90\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:59.369237
model: gpt-4-1106-preview
summary: "\u53D1\u9001\u5E26\u6709\u57FA\u672C\u8BA4\u8BC1\u7684HTTP\u8BF7\u6C42\uFF0C\
  \u5373\u7528\u7528\u6237\u540D\u548C\u5BC6\u7801\u901A\u8FC7\u7F51\u7EDC\u53D1\u9001\
  \u6570\u636E\u8BF7\u6C42\u3002\u7A0B\u5E8F\u5458\u8FD9\u4E48\u505A\u662F\u4E3A\u4E86\
  \u5B89\u5168\u4EA4\u6362\u6570\u636E\uFF0C\u8BBF\u95EE\u53D7\u4FDD\u62A4\u7684\u8D44\
  \u6E90\u3002"
title: "\u4F7F\u7528\u57FA\u672C\u8BA4\u8BC1\u53D1\u9001 HTTP \u8BF7\u6C42"
---

{{< edit_this_page >}}

## 什么 & 为什么？
发送带有基本认证的HTTP请求，即用用户名和密码通过网络发送数据请求。程序员这么做是为了安全交换数据，访问受保护的资源。

## 如何做：
```Arduino
#include <ESP8266WiFi.h>
#include <ESP8266HTTPClient.h>

const char* ssid = "your_SSID"; // 替换成你的网络名
const char* password = "your_PASSWORD"; // 替换成你的密码

const char* http_username = "your_username"; // HTTP认证用户名
const char* http_password = "your_password"; // HTTP认证密码

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(1000);
    Serial.println("Connecting to WiFi...");
  }

  HTTPClient http;
  http.begin("http://yourwebsite.com/data"); // 替换成你要请求的网址
  http.setAuthorization(http_username, http_password); // 设置基本认证

  int httpCode = http.GET();

  if (httpCode > 0) {
    String payload = http.getString();
    Serial.println(httpCode);
    Serial.println(payload);
  } else {
    Serial.println("Error on HTTP request");
  }

  http.end();
}

void loop() {
  // 这里不需要代码
}
```

输出应该显示HTTP状态码和请求的数据。

## 深入了解
发送带有基本认证的HTTP请求这个概忈在互联网早期就被广泛使用了。尽管它不如新兴的认证方式（如OAuth）安全，但因其实施简单，至今仍在许多场合被使用。Arduino通过WiFi库可以实现HTTP请求，而基本认证则通过HTTPClient库中的`setAuthorization`方法来设置。这适用于小型项目或快速原型，但对于更安全或更复杂的认证要求，则可能需要寻找其他方法。

## 参见
- ESP8266WiFi 库文档：https://arduino-esp8266.readthedocs.io/en/latest/esp8266wifi/readme.html
- ESP8266HTTPClient 库文档：https://arduino-esp8266.readthedocs.io/en/latest/esp8266httpclient/readme.html
- HTTP 状态码参考：https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Status
