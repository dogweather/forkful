---
date: 2024-01-20 17:59:20.196218-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-04-05T22:50:56.049810-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) \u6614\u306A\u304C\u3089\u306EWebClient\u3084HttpWebRequest\u3082\
  \u3042\u308B\u3051\u3069\u3001\u4ECA\u306FHttpClient\u304C\u4E3B\u6D41\u3002HttpClient\u306F\
  \u975E\u540C\u671F\u64CD\u4F5C\u306B\u6700\u9069\u5316\u3055\u308C\u3066\u3044\u3066\
  \u3001\u63A5\u7D9A\u306E\u518D\u5229\u7528\u3082\u8CE2\u3044\u3002\u4ED6\u306E\u65B9\
  \u6CD5\u306B\u306F\u3001RestSharp\u3084Flurl\u306A\u3069\u306E\u30E9\u30A4\u30D6\
  \u30E9\u30EA\u304C\u3042\u308B\u3002\u9078\u629E\u306F\u7528\u9014\u306B\u3088\u308B\
  \u3002\u5185\u90E8\u7684\u306B\u306F\u3001HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306F\
  TCP/IP\u30D7\u30ED\u30C8\u30B3\u30EB\u3092\u4F7F\u3063\u3066\u30C7\u30FC\u30BF\u3092\
  \u30B5\u30FC\u30D0\u306B\u9001\u308A\u3001\u30EC\u30B9\u30DD\u30F3\u30B9\u3092\u53D7\
  \u3051\u53D6\u308B\u4ED5\u7D44\u307F\u3002HTTP/1.1\u30D7\u30ED\u30C8\u30B3\u30EB\
  \u3067\u306F\u30B3\u30CD\u30AF\u30B7\u30E7\u30F3\u306E\u518D\u5229\u7528\u304C\u53EF\
  \u80FD\u3060\u3051\u3069\u3001\u65B0\u3057\u3044HTTP/2\u3067\u306F\u3055\u3089\u306B\
  \u52B9\u7387\u7684\u306A\u901A\u4FE1\u304C\u53EF\u80FD\u306B\u306A\u3063\u3066\u3044\
  \u308B\u3002"
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
