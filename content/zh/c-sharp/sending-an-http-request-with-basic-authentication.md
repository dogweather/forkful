---
title:                "使用基本身份验证发送http请求"
html_title:           "C#: 使用基本身份验证发送http请求"
simple_title:         "使用基本身份验证发送http请求"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# 为什么要发送基本身份验证的HTTP请求？
HTTP请求是通过Web进行通信的主要方法之一，基本身份验证可以为用户提供更安全的访问权限，防止未经授权的访问。

## 如何进行
首先，我们需要在C＃代码中导入System.Net命名空间，以便使用相关的类和方法。然后，我们可以使用下面的代码示例来创建一个带有基本身份验证的HTTP请求：

```C#
// 导入相关命名空间
using System.Net;
using System.IO;

// 创建HTTP请求
HttpWebRequest request = (HttpWebRequest)WebRequest.Create("http://example.com");
request.Method = "GET";

// 添加基本身份验证凭证
string credentials = Convert.ToBase64String(Encoding.ASCII.GetBytes("username:password"));
request.Headers["Authorization"] = "Basic " + credentials;

// 发送请求
HttpWebResponse response = (HttpWebResponse)request.GetResponse();

// 读取响应结果
string result;
using (StreamReader sr = new StreamReader(response.GetResponseStream()))
{
    result = sr.ReadToEnd();
}

// 输出结果
Console.WriteLine(result);
```

以上代码中，我们首先导入了System.Net命名空间，然后创建了一个HttpWebRequest对象，指定了请求的URL和方法。接着，我们使用Convert.ToBase64String方法将用户名和密码转换为Base64编码的字符串，并将其添加到请求的头部作为基本身份验证的凭证。最后，我们发送请求，读取响应结果，并输出到控制台。

## 深入了解
除了上述的基本示例之外，我们还可以使用NetworkCredential类来提供更多身份验证选项，如Digest身份验证。此外，我们还可以通过使用HttpClient类进行更灵活的HTTP请求操作。更多关于C＃中HTTP请求的信息，请参考以下链接：

- [MSDN: System.Net命名空间](https://docs.microsoft.com/zh-cn/dotnet/api/system.net?view=net-5.0)
- [MSDN: NetworkCredential类](https://docs.microsoft.com/zh-cn/dotnet/api/system.net.networkcredential?view=net-5.0)
- [MSDN: HttpClient类](https://docs.microsoft.com/zh-cn/dotnet/api/system.net.http.httpclient?view=net-5.0)

# See Also
- [贝塞尔: C#基本身份验证教程](https://www.cnblogs.com/2003h/p/9711119.html)
- [知乎: C#发送HTTP请求的几种常用方法](https://zhuanlan.zhihu.com/p/39410468)