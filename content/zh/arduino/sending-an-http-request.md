---
title:                "发出 HTTP 请求"
date:                  2024-01-20T17:58:56.283266-07:00
model:                 gpt-4-1106-preview
simple_title:         "发出 HTTP 请求"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? 什么 & 为什么?

发送HTTP请求是向服务器发数据或获取数据的方式；程序员这么做来交换网络上的信息，比如天气更新，社交媒体交云。

## How to: 怎么做

```Arduino
#include <ESP8266WiFi.h>

const char* ssid = "yourSSID"; // 替换为你的WiFi名称
const char* password = "yourPASSWORD"; // 替换为你的WiFi密码
const char* host = "jsonplaceholder.typicode.com"; 

void setup() {
  Serial.begin(115200);
  WiFi.begin(ssid, password);

  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.print(".");
  }
  Serial.println("WiFi connected");

  WiFiClient client;
  if (!client.connect(host, 80)) {
    Serial.println("Connection failed");
    return;
  }
  
  String url = "/posts/1";
  client.print(String("GET ") + url + " HTTP/1.1\r\n" +
               "Host: " + host + "\r\n" + 
               "Connection: close\r\n\r\n");

  while(client.available()){
    String line = client.readStringUntil('\r');
    Serial.print(line);
  }
}

void loop() {
  
}
```
输出样本:
```
HTTP/1.1 200 OK
...
{
  "userId": 1,
  "id": 1,
  "title": "sunt aut facere repellat provident occaecati excepturi optio reprehenderit",
  "body": "quia et suscipit\nsuscipit recusandae consequuntur expedita et cum\nreprehenderit..."
}
...
```
## Deep Dive: 深入探究

发送HTTP请求开始于90年代早期的Web。从那时起，HTTP成了Internet交流的基础。此方法比如GET和POST，现在用于各种应用。

使用Arduino发送HTTP请求时，得注意：
1. 客户端库：ESP8266WiFi库支持ESP8266模块。不同模块（比如ESP32）得用相应的库。
2. 内存限制：Arduino设备内存有限。编写代码时要注意内存管理。
3. 安全性：考虑使用HTTPS保护数据安全，但它要比HTTP更多消耗资源。

ESP8266/ESP32这样的WiFi模块让Arduino联网。选择模块时，考虑项目需求、成本、功率消耗。

## See Also: 相关资料

- [Arduino - WiFi](https://www.arduino.cc/en/Reference/WiFi)
- [HTTP Made Really Easy (James Marshall's Guide)](http://www.jmarshall.com/easy/http/)
