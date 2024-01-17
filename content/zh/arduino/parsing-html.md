---
title:                "解析HTML"
html_title:           "Arduino: 解析HTML"
simple_title:         "解析HTML"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/parsing-html.md"
---

{{< edit_this_page >}}

# 什么 & 为什么？
解析HTML是指将HTML代码转换为可读的格式，以便程序员能够提取和操作其中的数据。程序员通常会这样做是为了从网页中获取特定的信息，如价格、评分等等。

# 怎么做：
```
ArduinoHttpClient client; // 声明HTTP客户端
char server[] = "www.example.com"; // 设置目标网站
int port = 80; // 设置端口
if (client.connect(server, port)) { // 建立连接
    client.println("GET /index.html"); // 发送GET请求
    client.println("Host: www.example.com"); // 设置主机名
    client.println("Connection: close"); // 确保关闭连接
    client.println(); // 发送空行
}
while (client.available()) { // 循环读取响应
    char c = client.read(); // 读取一个字符
    // 处理响应，如打印到串口监视器
    Serial.print(c);
}
client.stop(); // 断开连接
```

输出的结果可能会是这样的：
```
HTTP/1.1 200 OK
Date: Wed, 06 Oct 2021 00:00:00 GMT
Server: Apache
Content-Type: text/html
Content-Length: 123

<!DOCTYPE html>
<html>
<head>
<title>Example Website</title>
</head>
<body>
<h1>Welcome to Example Website!</h1>
<p>Our prices start at $10 and our rating is 4 stars.</p>
</body>
</html>
```

# 深入了解：
解析HTML在计算机历史上是一个重要的技术。在Web爆发式增长的时期，解析器是将互联网上的信息转换为可读格式的关键工具。现在，有许多替代技术，如使用API接口来获取数据，但解析HTML仍然是一种重要的技能。

# 参考链接：
- [Wikipedia - HTML parsing](https://en.wikipedia.org/wiki/HTML_parsing)
- [Arduino Reference - HttpClient library](https://www.arduino.cc/reference/en/libraries/httpclient/)