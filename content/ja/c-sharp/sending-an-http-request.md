---
title:                "「httpリクエストを送信する」"
html_title:           "C#: 「httpリクエストを送信する」"
simple_title:         "「httpリクエストを送信する」"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## なぜ？

HTTPリクエストを送信する理由はいくつかあります。例えば、ウェブサイトからデータを取得する、外部のAPIに接続する、ウェブアプリケーションを作成するなどです。C#を使用することで、簡単かつ効率的にHTTPリクエストを送信することができます。

## ハウツー

```C#
using System;
using System.Net.Http;

class Program
{
    static async Task Main()
    {
        // HTTPクライアントを作成
        HttpClient client = new HttpClient();

        // リクエストを作成
        HttpRequestMessage request = new HttpRequestMessage(HttpMethod.Get, "https://example.com");

        // リクエストを送信し、レスポンスを受け取る
        HttpResponseMessage response = await client.SendAsync(request);

        // レスポンスのステータスコードを表示
        Console.WriteLine("Response status code: " + response.StatusCode);

        // レスポンスのボディを取得
        string responseBody = await response.Content.ReadAsStringAsync();

        // ボディを表示
        Console.WriteLine("Response body: " + responseBody);
    }
}
```

**出力:**

```
Response status code: 200
Response body: <html><head><title>Example Domain</title>...</html>
```

## ディープダイブ

HTTPリクエストを送信する場合、リクエストライブラリを使用することでより簡単にリクエストを作成することができます。また、リクエストのヘッダー、ボディ、クッキーなどのプロパティも設定することができます。さらに、異なるHTTPメソッドを使用することで、GET、POST、PUT、DELETEなどの異なるタイプのリクエストを送信することができます。

## 関連リンク

- [HttpClientクラス (System.Net.Http)](https://docs.microsoft.com/ja-jp/dotnet/api/system.net.http.httpclient)
- [GETリクエストの送信 (C#プログラムガイド)](https://docs.microsoft.com/ja-jp/dotnet/csharp/programming-guide/concepts/http-client/get-request)
- [HTTPメソッド (MDN Web Docs)](https://developer.mozilla.org/ja/docs/Web/HTTP/Methods)