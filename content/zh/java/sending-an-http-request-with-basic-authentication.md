---
title:                "使用基本认证发送http请求"
html_title:           "Java: 使用基本认证发送http请求"
simple_title:         "使用基本认证发送http请求"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 为什么
在编写Java程序时，我们经常会需要与外部服务器进行通信。使用HTTP请求是一种流行的方式，而基本身份验证可以帮助我们确保通信的安全性。

## 怎么做
我们可以使用Java自带的`HttpURLConnection`类来发送HTTP请求。首先，我们需要创建一个`URL`对象，指定目标服务器的地址。然后，我们可以使用`openConnection()`方法来打开连接。接着，我们可以使用`setRequestMethod()`方法来设置请求的方法，这里我们使用`GET`方法作为示例。最后，我们可以使用`setRequestProperty()`方法来指定需要的身份验证信息，例如用户名和密码。下面是一个完整的示例代码：
```Java
URL url = new URL("http://www.example.com/api");
HttpURLConnection connection = (HttpURLConnection) url.openConnection();
connection.setRequestMethod("GET");
connection.setRequestProperty("Authorization", "Basic dXNlcm5hbWU6cGFzc3dvcmQ=");
connection.connect();
```
这里我们使用Base 64编码来包装用户名和密码，以确保安全性。如果需要，我们也可以使用其他的身份验证方式，如摘要认证。

当请求成功发送后，我们可以通过`getResponseCode()`方法来获取服务器的响应状态码。可以通过`getInputStream()`方法来获取响应内容，或者通过`getErrorStream()`方法来获取错误信息。下面是一个简单的输出示例：
```Java
System.out.println(connection.getResponseCode()); //输出200
InputStream inputStream = connection.getInputStream();
Scanner scanner = new Scanner(inputStream);
while(scanner.hasNextLine()) {
  System.out.println(scanner.nextLine()); //输出服务器响应的内容
}
```

## 深入了解
基本身份验证是一种最简单的身份验证方式，但并不是最安全的。在实际的应用中，我们需要根据具体的需求来选择适合的身份验证方式。同时，我们也可以通过`setRequestProperty()`方法来设置其他的请求头信息，如`User-Agent`，这样可以模拟不同的浏览器或设备发送请求。另外，我们还可以使用`HttpsURLConnection`类来实现HTTPS连接，以达到更高的安全性。

## 同时也听听
- [Java官方文档：HttpURLConnection](https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html)
- [Java官方文档：HttpsURLConnection](https://docs.oracle.com/javase/8/docs/api/javax/net/ssl/HttpsURLConnection.html)
- [MDN文档：使用HTTP](https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Overview)