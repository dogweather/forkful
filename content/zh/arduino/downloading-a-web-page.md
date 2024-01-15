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

## 为什么

你是否曾经想过如何在你的Arduino项目中实现网页下载功能？下载网页可以让你获取互联网上的实时信息，并将其应用到你的项目中。在本文中，我们将学习如何使用Arduino来下载网页。

## 如何进行

首先，我们需要使用Arduino的WiFi模块来连接到一个无线网络。接下来，我们将使用WiFiClient库来创建一个客户端，以便与网络服务器建立连接。然后，我们可以使用这个客户端来发送HTTP GET请求，并将服务器的响应保存到一个字符串中。最后，我们可以通过串口来输出这个字符串，从而实现网页下载功能。下面是一个简单的代码示例：

```Arduino
#include <SPI.h>
#include <WiFiNINA.h>

char ssid[] = "YOUR_NETWORK_NAME";
char pass[] = "YOUR_NETWORK_PASSWORD";

WiFiClient client;

void setup() {
  Serial.begin(9600);
  while (!Serial) {
    ; // 等待串口连接
  }

  // 连接到WiFi网络
  Serial.print("连接到网络...");
  while (WiFi.begin(ssid, pass) != WL_CONNECTED) {
    // 等待连接成功
    delay(500);
    Serial.print(".");
  }
  Serial.println("连接成功！");

  // 输出网页内容
  Serial.println("正在下载网页...");
  if (client.connect("www.example.com", 80)) {
    // 发送GET请求
    client.println("GET /index.html HTTP/1.1");
    client.println("Host: www.example.com");
    client.println("Connection: close");
    client.println();
  }

  // 输出服务器响应
  while (client.available()) {
    char c = client.read();
    Serial.write(c);
  }
}

void loop() {
  // 该代码只能执行一次，因此可以省略此部分
}
```

例如，假设我们要下载Example网站的主页，可以在串口窗口中看到如下内容：

```
<!DOCTYPE html>
<html>
<head>
  <title>Example</title>
</head>
<body>
  <h1>Welcome to Example!</h1>
</body>
</html>
```

## 深入了解

要实现更复杂的网页下载功能，我们还可以使用WiFiSSLClient库来进行SSL连接，从而实现对HTTPS协议的支持。此外，我们还可以使用WebServer库和SD卡模块来创建自己的网络服务器，并从中获取数据。如果你想要深入了解如何在Arduino中进行网络通信，可以查看下面的相关资源。

## 查看也可

- [Arduino WiFiClient库文档](https://www.arduino.cc/en/Reference/WiFiClient)
- [Arduino WiFiSSLClient库文档](https://www.arduino.cc/en/Reference/WiFiSSLClient)
- [Arduino WebServer库文档](https://www.arduino.cc/en/Reference/WebServer)
- [Arduino SD卡库文档](https://www.arduino.cc/en/Reference/SD)