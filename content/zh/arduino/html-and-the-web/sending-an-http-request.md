---
date: 2024-01-20 17:58:56.283266-07:00
description: "\u53D1\u9001HTTP\u8BF7\u6C42\u662F\u5411\u670D\u52A1\u5668\u53D1\u6570\
  \u636E\u6216\u83B7\u53D6\u6570\u636E\u7684\u65B9\u5F0F\uFF1B\u7A0B\u5E8F\u5458\u8FD9\
  \u4E48\u505A\u6765\u4EA4\u6362\u7F51\u7EDC\u4E0A\u7684\u4FE1\u606F\uFF0C\u6BD4\u5982\
  \u5929\u6C14\u66F4\u65B0\uFF0C\u793E\u4EA4\u5A92\u4F53\u4EA4\u4E91\u3002"
isCJKLanguage: true
lastmod: '2024-03-11T00:14:21.858147-06:00'
model: gpt-4-1106-preview
summary: "\u53D1\u9001HTTP\u8BF7\u6C42\u662F\u5411\u670D\u52A1\u5668\u53D1\u6570\u636E\
  \u6216\u83B7\u53D6\u6570\u636E\u7684\u65B9\u5F0F\uFF1B\u7A0B\u5E8F\u5458\u8FD9\u4E48\
  \u505A\u6765\u4EA4\u6362\u7F51\u7EDC\u4E0A\u7684\u4FE1\u606F\uFF0C\u6BD4\u5982\u5929\
  \u6C14\u66F4\u65B0\uFF0C\u793E\u4EA4\u5A92\u4F53\u4EA4\u4E91\u3002"
title: "\u53D1\u51FA HTTP \u8BF7\u6C42"
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
