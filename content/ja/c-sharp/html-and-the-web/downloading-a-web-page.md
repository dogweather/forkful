---
date: 2024-01-20 17:43:52.739188-07:00
description: "How to: (\u3084\u308A\u65B9) \u5B9F\u884C\u3059\u308B\u3068\u3001\u4F8B\
  \u306E\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u6700\u521D\u306E100\u6587\u5B57\
  \u304C\u30B3\u30F3\u30BD\u30FC\u30EB\u306B\u8868\u793A\u3055\u308C\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.996579-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) \u5B9F\u884C\u3059\u308B\u3068\u3001\u4F8B\u306E\u30A6\
  \u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u6700\u521D\u306E100\u6587\u5B57\u304C\u30B3\
  \u30F3\u30BD\u30FC\u30EB\u306B\u8868\u793A\u3055\u308C\u307E\u3059\u3002"
title: "\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9"
weight: 42
---

## How to: (やり方)
```C#
using System;
using System.Net.Http;
using System.Threading.Tasks;

class Program
{
    static async Task Main()
    {
        var url = "https://example.com";
        using var client = new HttpClient();
        try
        {
            string content = await client.GetStringAsync(url);
            Console.WriteLine(content.Substring(0, 100)); // 最初の100文字を表示
        }
        catch (Exception e)
        {
            Console.WriteLine($"エラー: {e.Message}");
        }
    }
}
```
実行すると、例のウェブページの最初の100文字がコンソールに表示されます。

## Deep Dive (深堀り)
初期のウェブではHTTPリクエストはシンプルなテキストベースの通信でした。現在では、`HttpClient`クラスを使い、非同期的にページをダウンロードするのが一般的です。`WebClient`や`HttpWebRequest`などの古いクラスも使えますが、`HttpClient`の方がモダンな機能を提供します。`HttpClient`は接続の再利用やエラーハンドリングも扱いやすくしています。

## See Also (関連情報)
- [HttpClient Class](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient?view=net-6.0) - `HttpClient`クラスについての公式ドキュメント。
- [Asynchronous programming with async and await](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/async/) - C#の非同期プログラミングの概念について説明しています。
- [Performing HTTP requests in C#](https://docs.microsoft.com/en-us/dotnet/csharp/tutorials/console-webapiclient) - C#でHTTPリクエストを行う方法のチュートリアルです。
