---
title:                "使用基本认证发送http请求"
html_title:           "Bash: 使用基本认证发送http请求"
simple_title:         "使用基本认证发送http请求"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？ (What & Why?)
使用基本认证发送HTTP请求是一项技术，允许应用程序通过网络发送受保护的信息。程序员之所以使用它，主要是为了确保数据在互联网上的传输安全。

## 如何实现： (How to:) 
使用Java可以简单快速地实现HTTP请求加基本认证，下面以Java库OkHttp为例介绍一下实现步骤：
```java
//首先，导入必要的库：
import okhttp3.*;

public class OkHttpExample {
  public static void main(String[] args) throws Exception {
 	// 创建OkHttpClient
    OkHttpClient client = new OkHttpClient();

    // 添加请求认证
    String credentials = Credentials.basic("username", "password");

    // 创建请求对象
    Request request = new Request.Builder()
      .url("https://api.github.com/user")
      .header("Authorization", credentials)
      .build();

    try (Response response = client.newCall(request).execute()) {
      // 打印响应状态和响应体
      System.out.println(response.code() + " " + response.body().string());
    }
  }
}
```
运行以上代码将会输出如下内容（模拟输出，因为这取决于你的用户名和密码）：
```
200 {"login":"username","id":123456,...}
```
## 深入探究 (Deep Dive)
使用基础认证发送HTTP请求的历史可以追溯到HTTP/1.0的早期，那时候大家对网路安全的重视还不足。随着网路安全的越来越受到重视，HTTPS和更加安全的认证协议逐渐取代了基础认证。但是基础认证仍然在很多场景中被广泛使用，如简单应用、测试环境、内部系统等。

在Java中，除了OkHttp库之外，还有很多其他库可以实现HTTP请求，例如Apache HttpClient, HttpUrlConnection等。虽然实现方式上有些差异，但核心思想都是一样的。

在实现过程中，把用户名和密码加密编码后放到请求头是至关重要的一步，这样可以在信息在网络上传输的过程中保证其安全性。

## 另请参阅 (See Also)
- [OkHttp官方文档](https://square.github.io/okhttp/)
- [HttpUrlConnection官方文档](https://developer.android.com/reference/java/net/HttpURLConnection)