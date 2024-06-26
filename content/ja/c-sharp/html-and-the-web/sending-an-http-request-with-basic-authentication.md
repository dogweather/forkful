---
date: 2024-01-20 18:01:43.802632-07:00
description: "How to: (\u65B9\u6CD5) Basic\u8A8D\u8A3C\u306FHTTP\u30D7\u30ED\u30C8\
  \u30B3\u30EB\u306E\u6A19\u6E96\u7684\u306A\u8A8D\u8A3C\u30B9\u30AD\u30FC\u30E0\u3067\
  \u3001RFC 7617\u3067\u898F\u5B9A\u3055\u308C\u3066\u3044\u307E\u3059\u3002\u3053\
  \u308C\u306F\u3001`Authorization`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:41.664562-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) Basic\u8A8D\u8A3C\u306FHTTP\u30D7\u30ED\u30C8\u30B3\u30EB\
  \u306E\u6A19\u6E96\u7684\u306A\u8A8D\u8A3C\u30B9\u30AD\u30FC\u30E0\u3067\u3001RFC\
  \ 7617\u3067\u898F\u5B9A\u3055\u308C\u3066\u3044\u307E\u3059\u3002\u3053\u308C\u306F\
  \u3001`Authorization` \u30D8\u30C3\u30C0\u3067\u300C\u30E6\u30FC\u30B6\u30FC\u540D\
  :\u30D1\u30B9\u30EF\u30FC\u30C9\u300D\u306E\u30DA\u30A2\u3092Base64\u3067\u30A8\u30F3\
  \u30B3\u30FC\u30C9\u3057\u3066\u9001\u308A\u307E\u3059\u3002\u305F\u3060\u3057\u3001\
  HTTPS\u3092\u4F7F\u308F\u306A\u3044\u5834\u5408\u3001\u60C5\u5831\u306F\u6697\u53F7\
  \u5316\u3055\u308C\u305A\u3001\u7C21\u5358\u306B\u508D\u53D7\u3055\u308C\u308B\u5371\
  \u967A\u304C\u3042\u308B\u305F\u3081\u3001\u57FA\u672C\u7684\u306B\u306FHTTPS\u3068\
  \u7D44\u307F\u5408\u308F\u305B\u3066\u4F7F\u7528\u3059\u308B\u3079\u304D\u3067\u3059\
  \u3002"
title: "\u57FA\u672C\u8A8D\u8A3C\u3092\u4F7F\u7528\u3057\u305FHTTP\u30EA\u30AF\u30A8\
  \u30B9\u30C8\u306E\u9001\u4FE1"
weight: 45
---

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
