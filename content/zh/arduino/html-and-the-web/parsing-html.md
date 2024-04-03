---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:21.872514-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728Arduino\u4E0A\u89E3\u6790HTML\u901A\
  \u5E38\u8981\u6C42\u4F7F\u7528\u5360\u7528\u7A7A\u95F4\u5C0F\u7684\u5E93\uFF0C\u56E0\
  \u4E3A\u8BBE\u5907\u8D44\u6E90\u6709\u9650\u3002\u7528\u4E8E\u7F51\u7EDC\u6293\u53D6\
  \u548C\u89E3\u6790\u7684\u4E00\u4E2A\u6D41\u884C\u9009\u62E9\u662F\u4F7F\u7528`ESP8266HTTPClient`\u548C\
  `ESP8266WiFi`\u5E93\u9488\u5BF9ESP8266\uFF0C\u6216\u8005\u5B83\u4EEC\u7684ESP32\u5BF9\
  \u5E94\u7248\u672C\uFF0C\u9274\u4E8E\u5B83\u4EEC\u539F\u751F\u652F\u6301Wi-\u2026"
lastmod: '2024-03-13T22:44:48.059928-06:00'
model: gpt-4-0125-preview
summary: "\u5728Arduino\u4E0A\u89E3\u6790HTML\u901A\u5E38\u8981\u6C42\u4F7F\u7528\u5360\
  \u7528\u7A7A\u95F4\u5C0F\u7684\u5E93\uFF0C\u56E0\u4E3A\u8BBE\u5907\u8D44\u6E90\u6709\
  \u9650\u3002\u7528\u4E8E\u7F51\u7EDC\u6293\u53D6\u548C\u89E3\u6790\u7684\u4E00\u4E2A\
  \u6D41\u884C\u9009\u62E9\u662F\u4F7F\u7528`ESP8266HTTPClient`\u548C`ESP8266WiFi`\u5E93\
  \u9488\u5BF9ESP8266\uFF0C\u6216\u8005\u5B83\u4EEC\u7684ESP32\u5BF9\u5E94\u7248\u672C\
  \uFF0C\u9274\u4E8E\u5B83\u4EEC\u539F\u751F\u652F\u6301Wi-Fi\u529F\u80FD\u548CHTTP\u534F\
  \u8BAE\u3002\u4EE5\u4E0B\u662F\u4E00\u4E2A\u57FA\u7840\u793A\u4F8B\uFF0C\u4ECB\u7ECD\
  \u5982\u4F55\u83B7\u53D6\u548C\u89E3\u6790HTML\uFF0C\u5047\u8BBE\u4F60\u6B63\u5728\
  \u4F7F\u7528ESP8266\u6216ESP32\uFF1A\n\n\u9996\u5148\uFF0C\u5305\u542B\u5FC5\u8981\
  \u7684\u5E93\uFF1A."
title: "\u89E3\u6790HTML"
weight: 43
---

## 如何操作：
在Arduino上解析HTML通常要求使用占用空间小的库，因为设备资源有限。用于网络抓取和解析的一个流行选择是使用`ESP8266HTTPClient`和`ESP8266WiFi`库针对ESP8266，或者它们的ESP32对应版本，鉴于它们原生支持Wi-Fi功能和HTTP协议。以下是一个基础示例，介绍如何获取和解析HTML，假设你正在使用ESP8266或ESP32：

首先，包含必要的库：
```cpp
#include <ESP8266WiFi.h> // 适用于ESP8266
#include <ESP8266HTTPClient.h>
#include <WiFiClient.h>
// 如果使用ESP32，则使用类似的ESP32库

const char* ssid = "yourSSID";
const char* password = "yourPASSWORD";
```

连接到你的Wi-Fi网络：
```cpp
void setup() {
    Serial.begin(115200);
    WiFi.begin(ssid, password);

    while (WiFi.status() != WL_CONNECTED) {
        delay(1000);
        Serial.println("正在连接...");
    }
}
```

发出HTTP请求并解析一个简单的HTML片段：
```cpp
void loop() {
    if (WiFi.status() == WL_CONNECTED) { //检查WiFi连接状态
        HTTPClient http;  //声明一个HTTPClient类的对象

        http.begin("http://example.com");  //指定请求目的地
        int httpCode = http.GET();  //发送请求

        if (httpCode > 0) { //检查返回的代码
            String payload = http.getString();   //获取请求响应的有效载荷
            Serial.println(payload);             //打印响应有效载荷

            // 解析特定部分，例如，从有效载荷中提取标题
            int titleStart = payload.indexOf("<title>") + 7; // +7是为了跳过"<title>"标签
            int titleEnd = payload.indexOf("</title>", titleStart);
            String pageTitle = payload.substring(titleStart, titleEnd);

            Serial.print("页面标题：");
            Serial.println(pageTitle);
        }

        http.end();   //关闭连接
    }

    delay(10000); //每10秒发起一次请求
}
```

样本输出（假设http://example.com有一个简单的HTML结构）：
```
正在连接...
...
页面标题：Example Domain
```

该示例展示了如何获取一个HTML页面并提取`<title>`标签的内容。对于更复杂的HTML解析，考虑使用正则表达式（由于内存限制要小心使用）或字符串操作函数来导航HTML结构。高级解析可能需要更复杂的方法，包括针对您正在处理的HTML特定结构的自定义解析算法，因为标准的Arduino环境不包括内置的HTML解析库。
