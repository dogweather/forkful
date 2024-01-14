---
title:                "Java: 用基本认证发送一个HTTP请求"
simple_title:         "用基本认证发送一个HTTP请求"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

《为什么要发送带有基本认证的HTTP请求》
为什么:
如果您想要访问需要身份验证的资源，例如需要登录的网页，您就需要发送带有基本认证的HTTP请求来验证您的身份。

《如何发送带有基本认证的HTTP请求》
```Java
// 导入相关库
import java.io.IOException;
import org.apache.http.HttpEntity;
import org.apache.http.auth.AuthScope;
import org.apache.http.auth.UsernamePasswordCredentials;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.CredentialsProvider;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.BasicCredentialsProvider;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;

public class BasicAuthHTTPClient {
    public static void main(String[] args) throws Exception {
        // 创建CredentialsProvider对象并设置用户名和密码
        CredentialsProvider credsProvider = new BasicCredentialsProvider();
        credsProvider.setCredentials(
                new AuthScope("localhost", 8080),
                new UsernamePasswordCredentials("username", "password"));
        
        // 创建CloseableHttpClient对象并设置CredentialsProvider
        CloseableHttpClient httpclient = HttpClients.custom()
                .setDefaultCredentialsProvider(credsProvider)
                .build();
        
        try {
            // 创建HttpGet对象并设置请求地址
            HttpGet httpget = new HttpGet("http://localhost:8080/api/users");
            
            // 发送HTTP请求并获取响应
            System.out.println("Executing request " + httpget.getRequestLine());
            CloseableHttpResponse response = httpclient.execute(httpget);
            try {
                // 打印响应状态
                System.out.println("----------------------------------------");
                System.out.println(response.getStatusLine());
                System.out.println("----------------------------------------");

                // 获取响应实体
                HttpEntity entity = response.getEntity();

                // 打印响应内容
                if (entity != null) {
                    System.out.println(EntityUtils.toString(entity));
                }
            } finally {
                // 关闭响应
                response.close();
            }
        } finally {
            // 关闭HTTP client
            httpclient.close();
        }
    }
}
```
输出:
```text
Executing request GET http://localhost:8080/api/users HTTP/1.1
----------------------------------------
HTTP/1.1 200 OK
----------------------------------------
[{"id":1,"username":"John","email":"john@example.com"},{"id":2,"username":"Jane","email":"jane@example.com"}]
```

《深入解析发送带有基本认证的HTTP请求》
基本认证是HTTP协议中最简单的一种身份验证方式，它通过在请求头中添加"Authorization"字段来实现。具体而言，它的格式为"Basic username:password"，其中username和password以冒号为分隔符经过base64编码后放在请求头中。这种方法的缺点是，用户名和密码都是以明文传输，容易被窃取，因此在使用时应当慎重考虑。

《相关链接》
- [Apache HttpClient 4.x使用示例](https://hc.apache.org/httpcomponents-client-4.5.x/quickstart.html)
- [HTTP协议基本认证介绍](https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Authentication)
- [Base64编码介绍](https://zh.wikipedia.org/wiki/Base64)