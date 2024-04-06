---
date: 2024-01-20 17:58:56.283266-07:00
description: "How to: \u600E\u4E48\u505A \u53D1\u9001HTTP\u8BF7\u6C42\u5F00\u59CB\u4E8E\
  90\u5E74\u4EE3\u65E9\u671F\u7684Web\u3002\u4ECE\u90A3\u65F6\u8D77\uFF0CHTTP\u6210\
  \u4E86Internet\u4EA4\u6D41\u7684\u57FA\u7840\u3002\u6B64\u65B9\u6CD5\u6BD4\u5982\
  GET\u548CPOST\uFF0C\u73B0\u5728\u7528\u4E8E\u5404\u79CD\u5E94\u7528\u3002 \u4F7F\
  \u7528Arduino\u53D1\u9001HTTP\u8BF7\u6C42\u65F6\uFF0C\u5F97\u6CE8\u610F\uFF1A 1.\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.354567-06:00'
model: gpt-4-1106-preview
summary: "\u600E\u4E48\u505A \u53D1\u9001HTTP\u8BF7\u6C42\u5F00\u59CB\u4E8E90\u5E74\
  \u4EE3\u65E9\u671F\u7684Web\u3002\u4ECE\u90A3\u65F6\u8D77\uFF0CHTTP\u6210\u4E86\
  Internet\u4EA4\u6D41\u7684\u57FA\u7840\u3002\u6B64\u65B9\u6CD5\u6BD4\u5982GET\u548C\
  POST\uFF0C\u73B0\u5728\u7528\u4E8E\u5404\u79CD\u5E94\u7528."
title: "\u53D1\u51FA HTTP \u8BF7\u6C42"
weight: 44
---

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
