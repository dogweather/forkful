---
date: 2024-01-20 17:43:52.739188-07:00
description: "\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u3092\u30C0\u30A6\u30F3\u30ED\u30FC\
  \u30C9\u3059\u308B\u3053\u3068\u306F\u3001\u305D\u306E\u5185\u5BB9\u3092\u53D6\u5F97\
  \u3057\u3001\u4FDD\u5B58\u307E\u305F\u306F\u51E6\u7406\u3059\u308B\u3053\u3068\u3067\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u60C5\u5831\u306E\u81EA\
  \u52D5\u53CE\u96C6\u3001\u30D0\u30C3\u30AF\u30A2\u30C3\u30D7\u4F5C\u6210\u3001\u30B3\
  \u30F3\u30C6\u30F3\u30C4\u306E\u5206\u6790\u3084\u52A0\u5DE5\u306E\u305F\u3081\u306B\
  \u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:01.270118
model: gpt-4-1106-preview
summary: "\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u3092\u30C0\u30A6\u30F3\u30ED\u30FC\
  \u30C9\u3059\u308B\u3053\u3068\u306F\u3001\u305D\u306E\u5185\u5BB9\u3092\u53D6\u5F97\
  \u3057\u3001\u4FDD\u5B58\u307E\u305F\u306F\u51E6\u7406\u3059\u308B\u3053\u3068\u3067\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u60C5\u5831\u306E\u81EA\
  \u52D5\u53CE\u96C6\u3001\u30D0\u30C3\u30AF\u30A2\u30C3\u30D7\u4F5C\u6210\u3001\u30B3\
  \u30F3\u30C6\u30F3\u30C4\u306E\u5206\u6790\u3084\u52A0\u5DE5\u306E\u305F\u3081\u306B\
  \u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u30A6\u30A7\u30D6\u30DA\u30FC\u30B8\u306E\u30C0\u30A6\u30F3\u30ED\u30FC\u30C9"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
ウェブページをダウンロードすることは、その内容を取得し、保存または処理することです。プログラマーは、情報の自動収集、バックアップ作成、コンテンツの分析や加工のためにこれを行います。

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
