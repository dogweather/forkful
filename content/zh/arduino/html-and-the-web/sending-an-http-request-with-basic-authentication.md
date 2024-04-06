---
date: 2024-01-20 18:00:43.549718-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-04-05T22:51:01.272708-06:00'
model: gpt-4-1106-preview
summary: "\u5982\u4F55\u505A\uFF1A \u53D1\u9001\u5E26\u6709\u57FA\u672C\u8BA4\u8BC1\
  \u7684HTTP\u8BF7\u6C42\u8FD9\u4E2A\u6982\u5FC8\u5728\u4E92\u8054\u7F51\u65E9\u671F\
  \u5C31\u88AB\u5E7F\u6CDB\u4F7F\u7528\u4E86\u3002\u5C3D\u7BA1\u5B83\u4E0D\u5982\u65B0\
  \u5174\u7684\u8BA4\u8BC1\u65B9\u5F0F\uFF08\u5982OAuth\uFF09\u5B89\u5168\uFF0C\u4F46\
  \u56E0\u5176\u5B9E\u65BD\u7B80\u5355\uFF0C\u81F3\u4ECA\u4ECD\u5728\u8BB8\u591A\u573A\
  \u5408\u88AB\u4F7F\u7528\u3002Arduino\u901A\u8FC7WiFi\u5E93\u53EF\u4EE5\u5B9E\u73B0\
  HTTP\u8BF7\u6C42\uFF0C\u800C\u57FA\u672C\u8BA4\u8BC1\u5219\u901A\u8FC7HTTPClient\u5E93\
  \u4E2D\u7684`setAuthorization`\u65B9\u6CD5\u6765\u8BBE\u7F6E\u3002\u8FD9\u9002\u7528\
  \u4E8E\u5C0F\u578B\u9879\u76EE\u6216\u5FEB\u901F\u539F\u578B\uFF0C\u4F46\u5BF9\u4E8E\
  \u66F4\u5B89\u5168\u6216\u66F4\u590D\u6742\u7684\u8BA4\u8BC1\u8981\u6C42\uFF0C\u5219\
  \u53EF\u80FD\u9700\u8981\u5BFB\u627E\u5176\u4ED6\u65B9\u6CD5\u3002"
title: "\u4F7F\u7528\u57FA\u672C\u8BA4\u8BC1\u53D1\u9001 HTTP \u8BF7\u6C42"
weight: 45
---

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
