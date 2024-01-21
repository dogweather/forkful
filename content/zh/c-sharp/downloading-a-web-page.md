---
title:                "下载网页"
date:                  2024-01-20T17:43:42.304225-07:00
model:                 gpt-4-1106-preview
simple_title:         "下载网页"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
下载网页就是把网页内容保存到本地。程序员这么做是为了数据分析，备份信息，或是离线查看。

## How to: (怎么做：)
使用C#下载一个网页的基本流程。

```C#
using System;
using System.Net.Http;
using System.Threading.Tasks;

class Program
{
    static async Task Main(string[] args)
    {
        string url = "http://example.com"; // 目标网页
        HttpClient client = new HttpClient();
        
        try
        {
            string content = await client.GetStringAsync(url);
            Console.WriteLine(content); // 显示网页内容
        }
        catch (HttpRequestException e)
        {
            Console.WriteLine("网页下载出错: " + e.Message);
        }
    }
}
```
输出样例：
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

## Deep Dive (深入了解)
早期，下载网页常用 `WebClient` 类。现在，`HttpClient` 是首选，因为它更高效，支持异步操作。`GetStringAsync` 方法直接返回网页文本。还可以用 `GetAsync` 和 `ReadAsStringAsync` 提供更细节的控制。记得要处理网络错误和异常，确保代码健壮。

## See Also (另请参阅)
- [HttpClient Class](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient?view=net-6.0): 官方 `HttpClient` 类文档
- [Async and Await](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/async/): 关于C#异步编程的深入解析
- [Handling Exceptions](https://docs.microsoft.com/en-us/dotnet/csharp/fundamentals/exceptions/): 如何在C#中处理异常