---
title:                "发送http请求"
html_title:           "Arduino: 发送http请求"
simple_title:         "发送http请求"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 为什么

发送HTTP请求在Arduino编程中非常有用，因为通过网络连接，您可以获取外部数据或与外部服务进行交互。这使得您的项目更加动态和实用。

## 如何操作

要发送HTTP请求，您需要使用Arduino的Ethernet或WiFi库。首先，需要设置目标服务器的URL和端口。然后，使用`client.print()`来构建请求消息，包括方法和要发送的数据。最后，使用`client.available()`和`client.read()`来获取响应消息。

```Arduino
#include <Ethernet.h>

// 设置服务器的URL和端口
char server[] = "www.example.com";
int port = 80;

void setup() {
  // 初始化以太网
  Ethernet.begin(mac);

  // 打开串行通信
  Serial.begin(9600);
  while (!Serial) {
    ; // 等待串行端口连接
  }
}

void loop() {
  // 创建客户端对象
  EthernetClient client;
  // 连接服务器
  if (client.connect(server, port)) {
    // 发送请求消息
    client.print("GET /data HTTP/1.1\n");
    client.print("Host: www.example.com\n");
    client.print("Connection: close\n\n");
  }
  
  // 等待响应消息
  while(client.available()) {
    // 读取并打印响应消息
    char c = client.read();
    Serial.print(c);
  }
  client.stop(); // 断开连接
  
  // 等待5秒钟
  delay(5000);
}
```

## 深入探讨

发送HTTP请求涉及到构建和解析HTTP协议，这是一种网络通信协议，用于在客户端和服务器之间传输数据。HTTP协议的格式由请求行、请求头和请求体组成，其中最重要的部分是请求行，它包含请求方法、URL和协议版本。在Arduino中，我们可以使用字符串拼接和发送来构建请求消息，然后使用`client.read()`逐个字节读取响应消息，并根据协议来解析它。

## 参考链接

- Arduino HTTP请求指南：https://www.arduino.cc/en/Tutorial/WebClient
- HTTP协议介绍：https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview