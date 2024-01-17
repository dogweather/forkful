---
title:                "下载网页"
html_title:           "Arduino: 下载网页"
simple_title:         "下载网页"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

什么 & 为什么？
下载网页是指将网页内容从网络上获取到本地计算机的过程。程序员这么做是为了能够在本地分析和处理网页内容。

## 怎么做：
```
ArduinoHttpClient client;

client.get("https://www.example.com");

Serial.print(client.responseStatusCode());
Serial.println(client.responseBody());
```

## 深入了解：
下载网页的概念最早起源于互联网的早期，当时它主要是用来传输文本信息。今天，下载网页已经成为一项常见的技术，可用于在网页上获取各种信息，如图片、文本和数据。除了Arduino，程序员还可以使用其他编程语言和工具来实现下载功能，如Python和C++。

## 查看相关资源：
- [Arduino官方网站](https://www.arduino.cc)
- [ArduinoHttpClient文档](https://www.arduino.cc/en/Reference/HttpClient)