---
title:                "Arduino: 发送一个http请求"
simple_title:         "发送一个http请求"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

# 为什么：为什么要发送HTTP请求

发送HTTP请求是通过互联网与其他设备或服务器进行通信的一种常用方法。通过发送HTTP请求，您可以从另一个设备或服务器获取数据，也可以将数据发送到另一个设备或服务器。这种通信方式在Arduino编程中非常有用，因为它允许您与远程设备进行交互，从而实现更复杂的功能。

## 如何：发送HTTP请求的代码示例

要发送HTTP请求，首先需要定义一个客户端对象。然后，您可以使用该对象的`GET`方法来指定要发送请求的网址，并将响应存储在一个缓冲区中。接下来，您需要解析响应的内容并将其打印出来。下面是一个在Arduino中发送HTTP请求的示例代码：

```Arduino
#include <WiFi.h>
#include <WiFiClient.h>

WiFiClient client;

void setup() {
  Serial.begin(9600);
  // 连接到WiFi网络
  WiFi.begin("Your_Network_Name", "Your_Network_Password");

  while (WiFi.status() != WL_CONNECTED) {
    delay(500);
    Serial.println("Connecting to WiFi...");
  }
}

void loop() {
  int temperature = 23; // 假设要发送的数据为温度值为23

  // 定义网址，并将要发送的数据作为参数添加到网址中
  String url = "https://example.com/temperature?value=" + String(temperature);

  // 发送HTTP GET请求
  if (client.connect("example.com", 443)) {
    client.println("GET " + url + " HTTP/1.1");
    client.println("Host: example.com");
    client.println("Connection: close");
    client.println();
  }

  // 等待服务器响应
  while (client.connected()) {
    while (client.available()) {
      // 读取响应
      String line = client.readStringUntil('\r');
      Serial.println(line);
    }
  }
}

```

在上面的示例中，我们首先连接到WiFi网络，然后定义一个网址，并在网址中添加参数。最后，我们通过客户端对象发送GET请求，等待服务器响应并将其打印出来。

## 深入了解：发送HTTP请求的更多信息

发送HTTP请求还涉及到其他一些概念，例如网址编码和身份验证。网址编码是将特殊字符（例如空格和符号）转换为可在请求中使用的正确格式的过程。而身份验证是一种保护网络资源的方法，它要求您提供登录凭证才能访问特定的网址。

在Arduino编程中，您还可以使用更高级的库（例如`HTTPClient.h`）来轻松发送HTTP请求。此外，您还可以探索其他类型的HTTP请求（例如POST和PUT请求），从而实现更多功能。

## 参考链接

- [Arduino - WiFiClient](https://www.arduino.cc/en/Reference/WiFiClient)
- [Arduino - HTTPClient](https://github.com/arduino-libraries/ArduinoHttpClient)
- [Codecademy - What is HTTP?](https://www.codecademy.com/articles/http-requests)
- [Web Feeds - The History of HTTP](https://web.fe.up.pt/~jlopes/doku.php/teach/mm/livro/computer-networks-a-top-down-approach-6th-edition.pdf)

## 参见

- [使用Arduino进行WiFi通信的教程](https://blog.arduino.cc/2019/11/11/new-video-tutorial-wifi-communication-with-arduino/)