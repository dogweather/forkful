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

## 什么和为什么?

打开一个网页就是从互联网上取回特定URL的HTML代码。程序员这么做是为了编写能互动网络内容的应用。

## 如何做:

这是你如何用**ESP8266WiFi**类来下载网页的一个例子。

```Arduino
#include <ESP8266WiFi.h>
  
WiFiClient client;
  
void setup() { 
  Serial.begin(115200);
  WiFi.begin("your_network_name", "your_password");

  while (WiFi.status() != WL_CONNECTED) {
    delay(1000);
    Serial.print("Connecting..");
  }
  
  if (client.connect("www.the_page_you_want.com",80))
  {
    client.println("GET / HTTP/1.1");
    client.println("Host: www.the_page_you_want.com");
    client.println("Connection: close");
    client.println();
  }
}
  
void loop() {
  while(client.available()){
    String line = client.readStringUntil('\r');
    Serial.print(line);
  }
}
```
你会见到在串口监视器中将显示出下载了的HTML内容。

## 深入:

在早期,程序员下载网页时, 往往用CURL库或者wget命令。Arduino由于其引脚数量有限和缺少操作系统，需要特殊处理。所以有了ESP8266WiFi这个库。

有其他库和工具如HTTPClient库，你可以用它们来下载网页，但ESP8266WiFi比较简单直接。

实施细节方面，这代码中的“GET / HTTP/1.1”是HTTP协议的一部分。服务器接收到这个指令后会回应网页内容。

## 另请参阅:

你还可以查看以下链接以了解更多:

1. [Arduino WiFi library](https://www.arduino.cc/en/Reference/WiFi)
2. [Alternative Method: Using CURL](https://curl.haxx.se/)
3. [HTTP Protocol Basics](https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP)

提醒: 不要忘记替换代码中的 "your_network_name" 和 "your_password" 为你的WiFi信息，和替换 "www.the_page_you_want.com" 为你想下载的网页地址。