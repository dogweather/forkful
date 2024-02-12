---
title:                "基本認証を使用したHTTPリクエストの送信"
aliases: - /ja/c-sharp/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:01:43.802632-07:00
model:                 gpt-4-1106-preview
simple_title:         "基本認証を使用したHTTPリクエストの送信"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/sending-an-http-request-with-basic-authentication.md"
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
