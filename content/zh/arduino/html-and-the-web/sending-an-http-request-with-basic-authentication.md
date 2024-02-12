---
title:                "使用基本认证发送 HTTP 请求"
aliases: - /zh/arduino/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:00:43.549718-07:00
model:                 gpt-4-1106-preview
simple_title:         "使用基本认证发送 HTTP 请求"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/sending-an-http-request-with-basic-authentication.md"
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
