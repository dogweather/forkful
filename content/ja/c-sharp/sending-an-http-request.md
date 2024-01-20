---
title:                "HTTPリクエストの送信"
html_title:           "Bash: HTTPリクエストの送信"
simple_title:         "HTTPリクエストの送信"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 何となぜ？

HTTPリクエストの送信は、あるコンピュータがWebサーバーにデータを要求することを意味します。プログラマーは、情報を取得したり、サービスと対話したりするためにこれを行います。

## 方法:

以下に、C#でHTTPリクエストを送信する一例を示します。 

```C#
using System;
using System.Net.Http;

class Program
{
    static readonly HttpClient client = new HttpClient();
    
    static async Task Main()
    {
        HttpResponseMessage response = await client.GetAsync("http://example.com");

        response.EnsureSuccessStatusCode();
        string responseBody = await response.Content.ReadAsStringAsync();

        Console.WriteLine(responseBody);
    }
}
```

このコードは`http://example.com`にHTTPリクエストを送信し、応答をコンソールに表示します。

## ディープダイブ:

HTTPリクエストの送信は、1980年代からweb通信の基礎となってきました。C#では额許多なアプローチがありますが、最も現代的なアプローチは`HttpClient`を使用することです。

代替手段としては、`WebRequest`や`WebClient`クラスを使用する方法もありますが、これらは非推奨とされています。

また、非同期メソッドによる実装が推奨されます。これにより、アプリケーションがHTTPリクエストの応答を待つ間に他のタスクを処理することができます。

## 参考情報:

以下は、HTTPリクエストとC#の詳細についての追加情報が得られるリンク集です:

- [HttpClient Class (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)

- [HTTP (Wikipedia)](https://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol)