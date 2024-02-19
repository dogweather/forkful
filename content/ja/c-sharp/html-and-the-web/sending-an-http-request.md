---
aliases:
- /ja/c-sharp/sending-an-http-request/
date: 2024-01-20 17:59:20.196218-07:00
description: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u308B\u3063\u3066\u3001\
  \u30B5\u30FC\u30D0\u306B\u30C7\u30FC\u30BF\u3092\u8981\u6C42\u3057\u305F\u308A\u9001\
  \u4FE1\u3057\u305F\u308A\u3059\u308B\u3053\u3068\u3002\u306A\u305C\u304B\uFF1F\u30A6\
  \u30A7\u30D6\u306E\u60C5\u5831\u3092\u53D6\u5F97\u3057\u305F\u308A\u3001\u30A2\u30D7\
  \u30EA\u9593\u3067\u60C5\u5831\u3092\u3084\u308A\u53D6\u308A\u3059\u308B\u5FC5\u8981\
  \u304C\u3042\u308B\u304B\u3089\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:54.914793
model: gpt-4-1106-preview
summary: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u308B\u3063\u3066\u3001\u30B5\
  \u30FC\u30D0\u306B\u30C7\u30FC\u30BF\u3092\u8981\u6C42\u3057\u305F\u308A\u9001\u4FE1\
  \u3057\u305F\u308A\u3059\u308B\u3053\u3068\u3002\u306A\u305C\u304B\uFF1F\u30A6\u30A7\
  \u30D6\u306E\u60C5\u5831\u3092\u53D6\u5F97\u3057\u305F\u308A\u3001\u30A2\u30D7\u30EA\
  \u9593\u3067\u60C5\u5831\u3092\u3084\u308A\u53D6\u308A\u3059\u308B\u5FC5\u8981\u304C\
  \u3042\u308B\u304B\u3089\u3002"
title: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u9001\u4FE1"
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
