---
title:                "基本認証を使用してhttpリクエストを送信する"
html_title:           "C#: 基本認証を使用してhttpリクエストを送信する"
simple_title:         "基本認証を使用してhttpリクエストを送信する"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 何となぜ？

HTTPリクエストにBasic認証を含むことは、特定のプロトコルを使用してリソースを安全にアクセスする方法です。プログラマーは、ユーザーIDとパスワードを通じてサーバーへのアクセスを制御するためにこれを行います。

## どうするの？

下記はC#でBasic認証を使用してHTTPリクエストを送信する基本的な例です。

```C#
using (var client = new HttpClient())
{
    var byteArray = Encoding.ASCII.GetBytes("username:password");
    client.DefaultRequestHeaders.Authorization =
        new System.Net.Http.Headers.AuthenticationHeaderValue("Basic", Convert.ToBase64String(byteArray));

    var response = await client.GetAsync("http://targeturl.com");
    Console.WriteLine(await response.Content.ReadAsStringAsync());
}
```
このコードが正常に動作すると、指定したURLから返されたすべてのテキストがコンソールに表示されます。

## ディープダイブ

1. 歴史的背景: Basic認証はHTTP/1.0時代から存在しており、今日でも広く利用されています。
2. 代替手段: OAuth、Bearerトークン、APIキーなど、他の認証方法も存在します。
3. 実装詳細: Basic認証はBase64エンコーディングを使用してユーザー名とパスワードを伝送しますが、これは脆弱性があるため、HTTPSなどの安全な接続でのみ使用することを推奨します。

## 同関連ソース

-  [HttpClient クラス (System.Net.Http)](https://docs.microsoft.com/ja-jp/dotnet/api/system.net.http.httpclient?view=net-5.0)
-  [AuthenticationHeaderValue クラス (System.Net.Http.Headers)](https://docs.microsoft.com/ja-jp/dotnet/api/system.net.http.headers.authenticationheadervalue?view=net-5.0)
-  [HTTP Basic認証](https://tools.ietf.org/html/rfc7617)