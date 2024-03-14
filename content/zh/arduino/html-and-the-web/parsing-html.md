---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:21.872514-07:00
description: "\u5728Arduino\u9879\u76EE\u4E2D\u89E3\u6790HTML\u662F\u6307\u4ECE\u7F51\
  \u9875\u4E2D\u63D0\u53D6\u4FE1\u606F\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\
  \u4E3A\u4E86\u4F7F\u4ED6\u4EEC\u7684Arduino\u8BBE\u5907\u80FD\u591F\u4E0E\u4E92\u8054\
  \u7F51\u4EA4\u4E92\uFF0C\u4ECE\u7F51\u7AD9\u6536\u96C6\u6570\u636E\uFF0C\u7528\u9014\
  \u8303\u56F4\u4ECE\u5BB6\u5EAD\u81EA\u52A8\u5316\u5230\u73AF\u5883\u76D1\u6D4B\u3002"
lastmod: '2024-03-13T22:44:48.059928-06:00'
model: gpt-4-0125-preview
summary: "\u5728Arduino\u9879\u76EE\u4E2D\u89E3\u6790HTML\u662F\u6307\u4ECE\u7F51\u9875\
  \u4E2D\u63D0\u53D6\u4FE1\u606F\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\
  \u4E86\u4F7F\u4ED6\u4EEC\u7684Arduino\u8BBE\u5907\u80FD\u591F\u4E0E\u4E92\u8054\u7F51\
  \u4EA4\u4E92\uFF0C\u4ECE\u7F51\u7AD9\u6536\u96C6\u6570\u636E\uFF0C\u7528\u9014\u8303\
  \u56F4\u4ECE\u5BB6\u5EAD\u81EA\u52A8\u5316\u5230\u73AF\u5883\u76D1\u6D4B\u3002"
title: "\u89E3\u6790HTML"
---

{{< edit_this_page >}}

## 什么 & 为什么？

在Arduino项目中解析HTML是指从网页中提取信息。程序员这样做是为了使他们的Arduino设备能够与互联网交互，从网站收集数据，用途范围从家庭自动化到环境监测。

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
