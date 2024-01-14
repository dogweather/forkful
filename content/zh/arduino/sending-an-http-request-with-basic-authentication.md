---
title:                "Arduino: 使用基本身份验证发送http请求"
simple_title:         "使用基本身份验证发送http请求"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

##为什么：
为什么要使用Arduino发出带有基本身份验证的HTTP请求？基本身份验证是一种保护您的网络通信的方法，它通过要求用户提供用户名和密码来验证用户的身份。通过在Arduino中发送带有基本身份验证的HTTP请求，您可以保护您的设备免受未经授权的访问。

##如何：
要在Arduino中发送带有基本身份验证的HTTP请求，请按照以下步骤操作：

1. 首先，在您的Arduino项目中包含WiFi库和HTTPClient库。
```
#include <WiFi.h>
#include <HTTPClient.h>
```

2. 连接到您的WiFi网络。请确保您的WiFi名称和密码与您的网络相匹配。
```
char *ssid = "Your WiFi Network Name";
char *password = "Your WiFi Password";

WiFi.begin(ssid, password);
while (WiFi.status() != WL_CONNECTED) {
  delay(1000);
  Serial.println("Connecting to WiFi..");
}
```

3. 创建HTTPClient对象，并设置您要发送请求的URL。
```
HTTPClient http;
http.begin("https://www.example.com/api"); // replace with your URL
```

4. 添加基本身份验证，通过在请求头中包含用户名和密码。请注意，您必须将用户名和密码替换为您自己的。
```
http.addHeader("Authorization", "Basic YWRtaW46cGFzc3dvcmQ="); // replace with your own username and password
```

5. 发送GET请求，并获取响应。
```
int httpResponseCode = http.GET();
String response = http.getString();
Serial.println(httpResponseCode); // prints the response code
Serial.println(response); // prints the response body
```

6. 断开连接并等待一段时间，然后重新启动Arduino。
```
http.end();
delay(10000);
ESP.restart();
```

##深入探讨：
基本身份验证是一种最简单的身份验证方法，但它并不是最安全的。在发送HTTP请求时，所有的数据都是以明文形式传输的，这意味着它们可以被截获和解密。为了提高安全性，您可以使用其他方法，如OAuth身份验证。此外，您还可以使用SSL / TLS来加密您的通信，从而进一步保护您的设备免受攻击。

##另请参阅：
- [Arduino WiFi library documentation](https://www.arduino.cc/en/Reference/WiFi)
- [HTTPClient library documentation](https://github.com/espressif/arduino-esp32/tree/master/libraries/HTTPClient)
- [Basic authentication in HTTP](https://www.digitalocean.com/community/tutorials/http-basic-authentication-vs-oauth-1-2)
- [Secure communication with SSL/TLS](https://www.cloudflare.com/learning/ssl/what-is-ssl-tls/)