---
title:                "发送一个HTTP请求"
html_title:           "Arduino: 发送一个HTTP请求"
simple_title:         "发送一个HTTP请求"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 什么是发送HTTP请求？
发送HTTP请求是指通过互联网向服务器发送一个请求，以获取服务器上的数据或执行特定的操作。程序员经常使用HTTP请求来实现与网络服务的交互，例如获取网页内容、发送电子邮件或下载文件。

## 如何发送HTTP请求：
其中最常用的方法是使用Arduino的WiFiClient库来建立一个HTTP客户端。首先，你需要设置WiFi网络连接，然后将服务器的IP地址和端口号填入代码中。接下来，使用“GET”或“POST”方法来定义你想要发送的请求，然后使用write方法来向服务器发送请求。最后，使用readString方法来读取服务器的响应。

```
Arduino WiFiClient client;

// connect to WiFi network
WiFi.begin(ssid, pass);

// server's IP address and port number
IPAddress serverIP(192,168,1,1);
int serverPort = 80;

// send a GET request
client.write("GET /index.html HTTP/1.1\r\n");
client.write("Host: serverIP\r\n");
client.write("\r\n");

// read server's response
String response = client.readString();
```

## 深入了解：
HTTP请求是由蒂姆·伯纳斯-李（Tim Berners-Lee）在1991年发明的，在当今的互联网中被广泛使用。除了上面提到的WiFiClient库，程序员也可以使用其他库来发送HTTP请求，例如ESP8266HTTPClient库。此外，HTTP请求也可以通过手动编写数据包来实现发送。最后，值得一提的是，HTTPS协议提供了更安全的HTTP请求发送方式，它使用SSL加密来保护数据的传输。

## 参考：
- [Arduino WiFiClient Library Reference](https://www.arduino.cc/en/Reference/WiFiClient)
- [ESP8266HTTPClient Library Reference](https://arduino-esp8266.readthedocs.io/en/latest/esp8266wifi/esp8266wifi-client-secure-client.html)
- [Hypertext Transfer Protocol - Wikipedia](https://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol)