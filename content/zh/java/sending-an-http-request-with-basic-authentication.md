---
title:                "发送带有基本认证的http请求。"
html_title:           "Java: 发送带有基本认证的http请求。"
simple_title:         "发送带有基本认证的http请求。"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 什么是基本身份验证
发送HTTP请求时，基本身份验证是一种在请求头中包含用户名和密码的方法，用于验证用户身份。程序员经常使用基本身份验证来保护敏感信息和限制访问权限。

## 如何发送带有基本身份验证的HTTP请求
下面是一个简单的示例，说明如何在Java中发送带有基本身份验证的HTTP请求：
```
// 导入必要的包
import java.net.URL;
import java.net.HttpURLConnection;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;

// 创建一个实现基本身份验证的类
public class BasicAuthExample {
    public static void main(String[] args) throws IOException {

        // 定义API的URL和用户名，密码
        String apiURL = "https://example.com/api";
        String username = "username";
        String password = "password";

        // 创建URL对象，并为HTTP连接设置基本身份验证
        URL url = new URL(apiURL);
        HttpURLConnection con = (HttpURLConnection) url.openConnection();
        con.setRequestProperty("Authorization", "Basic " + Base64.getEncoder().encodeToString((username + ":" + password).getBytes()));

        // 发送HTTP GET请求
        con.setRequestMethod("GET");
        int status = con.getResponseCode();

        // 读取HTTP响应
        BufferedReader in = new BufferedReader(
                new InputStreamReader(con.getInputStream()));
        String inputLine;
        StringBuffer content = new StringBuffer();
        while ((inputLine = in.readLine()) != null) {
            content.append(inputLine);
        }
        in.close();

        // 打印响应结果
        System.out.println("Status code: " + status);
        System.out.println("Response: " + content.toString());

        // 关闭连接
        con.disconnect();
    }
}
```
该例子首先导入必要的包，包括`URL`、`HttpURLConnection`和`Base64`。然后创建一个名为`BasicAuthExample`的类，并在`main`方法中定义API的URL、用户名和密码。

在发送HTTP请求前，需要先创建一个`URL`对象，并使用`openConnection()`方法创建一个`HttpURLConnection`连接。然后使用`setRequestProperty()`方法，将包含用户名和密码的基本身份验证添加到请求头中。最后，使用`setRequestMethod()`方法指定HTTP请求的方法，这里使用`GET`方法发送请求。

接下来，使用`getResponseCode()`方法获取HTTP响应的状态码，并使用`getInputStream()`方法读取响应内容。最后，通过打印响应状态码和内容来验证请求是否成功。

## 深入研究
基本身份验证机制最早出现在HTTP/1.0协议中，是一种简单且广泛使用的身份验证方法。但是，由于它的缺点（如明文传输密码），现在很少被单独使用。通常程序员会使用其他安全协议（如HTTPS）来保护用户的身份信息。

除了基本身份验证之外，还有其他身份验证方法，如摘要身份验证（Digest Authentication）和OAuth 2.0，它们提供更安全的身份验证方式来保护用户信息。

## 参考文献
- [HTTP Basic and Digest Authentication Schemes](https://www.rfc-editor.org/rfc/rfc7617.html)
- [HTTP Authentication: Basic and Digest Access Authentication](https://www.rfc-editor.org/rfc/rfc2617.html)
- [Basic Access Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_access_authentication)