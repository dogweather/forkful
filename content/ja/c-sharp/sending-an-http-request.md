---
title:                "HTTPリクエストの送信"
date:                  2024-01-20T17:59:20.196218-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTPリクエストの送信"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
HTTPリクエストを送るって、サーバにデータを要求したり送信したりすること。なぜか？ウェブの情報を取得したり、アプリ間で情報をやり取りする必要があるから。

## How to: (方法)
```C#
// HttpClientを使ってみよう。必要なのはusingディレクティブだけ。
using System;
using System.Net.Http;
using System.Threading.Tasks;

class Program
{
    static async Task Main(string[] args)
    {
        using (HttpClient client = new HttpClient())
        {
            // GETリクエストを送信。
            HttpResponseMessage response = await client.GetAsync("http://example.com");
            response.EnsureSuccessStatusCode();
            string responseBody = await response.Content.ReadAsStringAsync();

            // 結果をコンソールに表示。
            Console.WriteLine(responseBody);
        }
    }
}
```
出力 (sample output):
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
    ...
</html>
```

## Deep Dive (深掘り)
昔ながらのWebClientやHttpWebRequestもあるけど、今はHttpClientが主流。HttpClientは非同期操作に最適化されていて、接続の再利用も賢い。他の方法には、RestSharpやFlurlなどのライブラリがある。選択は用途による。内部的には、HTTPリクエストはTCP/IPプロトコルを使ってデータをサーバに送り、レスポンスを受け取る仕組み。HTTP/1.1プロトコルではコネクションの再利用が可能だけど、新しいHTTP/2ではさらに効率的な通信が可能になっている。

## See Also (関連情報)
- Microsoft Docs – HttpClient Class: https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient
- Introduction to REST and .NET Core: https://docs.microsoft.com/en-us/aspnet/core/tutorials/first-web-api
- HTTP/2 Explained: https://http2.github.io/faq/