---
date: 2024-01-20 17:59:20.196218-07:00
description: "How to: (\u65B9\u6CD5) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.119059-06:00'
model: gpt-4-1106-preview
summary: .
title: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u9001\u4FE1"
weight: 44
---

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
