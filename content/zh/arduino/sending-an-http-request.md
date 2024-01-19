---
title:                "发送http请求"
html_title:           "C#: 发送http请求"
simple_title:         "发送http请求"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 什么和为什么?
HTTP请求是一个典型的计算机网络通信模式. 程序员用HTTP请求来交换数据，以便诸如温度传感器这样的设备可以和远程服务器分享信息。

## 如何实现:
我们首先需要连接一个WiFi网络，然后使用HTTP客户端发送请求到服务器。下面是一个样例代码：

```Arduino
#include <ESP8266WiFi.h>
#include <ESP8266HTTPClient.h>
 
const char* ssid = "your_SSID";
const char* password = "your_PASSWORD";
 
void setup () {
 
  Serial.begin(115200);
  WiFi.begin(ssid, password);
 
  while (WiFi.status() != WL_CONNECTED) {
 
    delay(1000);
    Serial.println("Connecting...");
  }
}
 
void loop() {
 
  if (WiFi.status() == WL_CONNECTED) {
 
    HTTPClient http;
 
    http.begin("http://jsonplaceholder.typicode.com/comments?id=10");
    int httpCode = http.GET();
 
    if (httpCode > 0) { 
 
      String payload = http.getString();
      Serial.println(payload);
    }
    http.end();
  }
  delay(30000);
}
```
样例输出：

```
{
  "postId": 2,
  "id": 10,
  "name": "eaque et deleniti atque tenetur ut quo ut",
  "email": "Carmen_Keeling@caroline.name",
  "body": "voluptate iusto quis nobis reprehenderit ipsum amet nulla quia quas dolores velit et non aut quia necessitatibus nostrum deserunt eius est sit sed veniam"
}
```

## 深入探索
HTTP请求在web应用中被广泛使用，其历史可以追溯到1980年代。虽然有其他方法，如FTP，SMTP，但HTTP请求在获取Web资源方面最为常见。除了WiFi和HTTPClient外，还有其他实现方式，如Ethernet和HttpClient等库。

## 参考资料
1. “HTTP介绍” - Mozilla开发者网络 (https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Overview)
2. "ESP8266WiFi库文档" - Arduino官方文档 (https://arduino-esp8266.readthedocs.io/en/latest/esp8266wifi/readme.html)
3. “Arduino HttpClient库” - Arduino官方文档 (https://www.arduino.cc/en/Tutorial/LibraryExamples/HttpClient)