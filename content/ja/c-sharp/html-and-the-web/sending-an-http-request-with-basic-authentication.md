---
date: 2024-01-20 18:01:43.802632-07:00
description: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092Basic\u8A8D\u8A3C\u3067\u9001\
  \u308B\u306E\u306F\u3001\u30B5\u30FC\u30D0\u30FC\u3078\u30BB\u30AD\u30E5\u30A2\u306A\
  \u65B9\u6CD5\u3067\u30E6\u30FC\u30B6\u30FC\u8A8D\u8A3C\u60C5\u5831\u3092\u4F1D\u3048\
  \u308B\u30D7\u30ED\u30BB\u30B9\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306F\u3053\u306E\u65B9\u6CD5\u3092\u4F7F\u3044\u3001\u8A8D\u8A3C\u304C\u5FC5\u8981\
  \u306AAPI\u3084\u30EA\u30BD\u30FC\u30B9\u3078\u5B89\u5168\u306B\u30A2\u30AF\u30BB\
  \u30B9\u3057\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.122780-06:00'
model: gpt-4-1106-preview
summary: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092Basic\u8A8D\u8A3C\u3067\u9001\u308B\
  \u306E\u306F\u3001\u30B5\u30FC\u30D0\u30FC\u3078\u30BB\u30AD\u30E5\u30A2\u306A\u65B9\
  \u6CD5\u3067\u30E6\u30FC\u30B6\u30FC\u8A8D\u8A3C\u60C5\u5831\u3092\u4F1D\u3048\u308B\
  \u30D7\u30ED\u30BB\u30B9\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\
  \u3053\u306E\u65B9\u6CD5\u3092\u4F7F\u3044\u3001\u8A8D\u8A3C\u304C\u5FC5\u8981\u306A\
  API\u3084\u30EA\u30BD\u30FC\u30B9\u3078\u5B89\u5168\u306B\u30A2\u30AF\u30BB\u30B9\
  \u3057\u307E\u3059\u3002"
title: "\u57FA\u672C\u8A8D\u8A3C\u3092\u4F7F\u7528\u3057\u305FHTTP\u30EA\u30AF\u30A8\
  \u30B9\u30C8\u306E\u9001\u4FE1"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
HTTPリクエストをBasic認証で送るのは、サーバーへセキュアな方法でユーザー認証情報を伝えるプロセスです。プログラマーはこの方法を使い、認証が必要なAPIやリソースへ安全にアクセスします。

## How to: (方法)
```c#
using System;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Text;
using System.Threading.Tasks;

class Program
{
    static async Task Main()
    {
        var client = new HttpClient();
        var uri = "https://example.com/api/data";
        var username = "your_username";
        var password = "your_password";

        client.DefaultRequestHeaders.Authorization = 
            new AuthenticationHeaderValue("Basic", Convert.ToBase64String(Encoding.ASCII.GetBytes($"{username}:{password}")));

        try
        {
            var response = await client.GetAsync(uri);
            if (response.IsSuccessStatusCode)
            {
                var content = await response.Content.ReadAsStringAsync();
                Console.WriteLine(content);
            }
            else
            {
                Console.WriteLine($"Error: {response.StatusCode}");
            }
        }
        catch(Exception e)
        {
            Console.WriteLine(e.Message);
        }
    }
}

// Sample Output:
// {"data": "Your secure info!"}
```

## Deep Dive (詳細情報)
Basic認証はHTTPプロトコルの標準的な認証スキームで、RFC 7617で規定されています。これは、`Authorization` ヘッダで「ユーザー名:パスワード」のペアをBase64でエンコードして送ります。ただし、HTTPSを使わない場合、情報は暗号化されず、簡単に傍受される危険があるため、基本的にはHTTPSと組み合わせて使用するべきです。

代替手段としてOAuthなどのよりセキュアな認証スキームがありますが、シンプルなシステムや古いアプリケーションでまだBasic認証が使われることがあります。データの機密性が非常に高い場合は、より強力な認証方式の使用を検討するべきです。

最新の.NETのHttpClientは、認証ヘッダーの設定やエラーハンドリングを簡単にする機能を提供します。安全性を保つために、パスワードなどの機密情報はハードコードせずに、安全な方法で管理することが重要です。

## See Also (関連情報)
- [RFC 7617 - The 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617)
- [HttpClient Class in .NET](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)
- [Basic Authentication on MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme)
