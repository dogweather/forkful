---
title:                "C#: 发送http请求"
simple_title:         "发送http请求"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 为什么

在编写程序时，您可能会遇到需要发送HTTP请求的情况。通过发送HTTP请求，您可以从其他网站或服务中获取数据，例如API。这可以让您的程序变得更加功能强大，为用户提供更好的体验。

## 如何

要发送HTTP请求，您需要使用C#中的HttpClient类。首先，您需要在程序中引入System.Net.Http命名空间。然后，创建一个新的HttpClient实例，并指定您要发送请求的URL。接下来，您需要指定所需的HTTP方法，如GET、POST或PUT，并添加所需的任何参数。最后，使用SendAsync方法发送请求，并等待响应。以下是一个简单的示例，向Google发送GET请求并打印响应的内容：

```C#
using System;
using System.Net.Http;

namespace HelloWorld
{
    class Program
    {
        static void Main(string[] args)
        {
            using (HttpClient client = new HttpClient())
            {
                // 发送一个GET请求，获取www.google.com的主页内容
                HttpResponseMessage response = client.GetAsync("https://www.google.com").Result;

                // 打印响应内容
                Console.WriteLine(response.Content.ReadAsStringAsync().Result);
            }
        }
    }
}
```

输出：

```
<!doctype html> <html itemscope="" ... // 此处省略了Google主页的HTML内容
```

## 深入了解

除了基本的HTTP请求方法外，您还可以使用HttpClient类来设置请求头、处理重定向、验证SSL证书等。此外，您还可以使用HttpContent类来发送POST请求，并将任何数据发送到服务器。了解这些功能可以帮助您更有效地发送HTTP请求，并根据您的需求进行定制。

## 参考资料

- [C# HttpClient文档](https://docs.microsoft.com/zh-cn/dotnet/api/system.net.http.httpclient)
- [HTTP请求方法](https://developer.mozilla.org/zh-CN/docs/Web/HTTP/Methods)
- [使用C#发送HTTP请求](https://www.codementor.io/@mirko0/how-to-make-http-requests-in-c-phnk59pce)（英文）