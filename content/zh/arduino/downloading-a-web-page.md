---
title:                "Arduino: '下载网页'"
simple_title:         "'下载网页'"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/downloading-a-web-page.md"
---

{{< edit_this_page >}}

Mandarin Blog Post: 下载网页与Arduino编程

## 为什么

有时候我们需要从网上获取信息或者通过互联网控制设备。通过编写Arduino代码，我们可以轻松地实现这些功能。在本文中，我们将探讨如何使用Arduino下载网页，并给出一些实用的代码示例。

## 如何做

首先，我们需要使用Ethernet库来连接网络。然后，我们可以使用Client库中的"web page"示例来下载网页。代码示例如下：

```Arduino
#include <Ethernet.h>
char server[] = "www.example.com"; // 替换为你想要下载的网页地址
EthernetClient client;

void setup()
{
  Serial.begin(9600);
  while (!Serial)
  {
    ; // 连接串口
  }
  // 开始网络连接
  if (Ethernet.begin(mac) == 0) {
    Serial.println("Failed to configure Ethernet using DHCP");
    // 如果DHCP失败，请使用静态IP地址
  }
  // 打印IP地址
  Serial.print("IP Address: ");
  Serial.println(Ethernet.localIP());
}

void loop() {
  // 发起连接
  if (client.connect(server, 80)) {
    Serial.println("connected");
    // 发送HTTP请求
    client.println("GET /index.html HTTP/1.1");
    client.println("Host: www.example.com");
    client.println("Connection: close");
    client.println();
  } else {
    // 连接失败
    Serial.println("connection failed");
  }
  // 读取服务器响应并打印到串口
  while (client.connected()) {
    if (client.available()) {
      char c = client.read();
      Serial.print(c);
    }
  }
  // 断开连接
  client.stop();
}

```

运行以上代码后，你将在串口监视器中看到网页的HTML代码。

## 深入探讨

上述示例代码只是简单地下载网页的一部分内容。实际上，我们可以通过HTTP GET请求来获取更多的信息，例如响应头部、页面跳转等。此外，我们还可以通过使用其他库来实现更复杂的功能，例如解析JSON格式数据，或者通过POST请求来发送数据到网页。

使用Arduino下载网页还有一个实际的应用，就是可以通过互联网控制你的设备。通过编写Arduino代码，你可以从网页上发送命令到设备，实现远程控制的功能。

## 另请参阅

- [Ethernet库文档](https://www.arduino.cc/en/Reference/Ethernet)

- [Client库文档](https://www.arduino.cc/en/Reference/Client)

- [Arduino官方示例代码](https://www.arduino.cc/en/Tutorial/WebClient)