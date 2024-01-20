---
title:                "ウェブページのダウンロード"
html_title:           "Bash: ウェブページのダウンロード"
simple_title:         "ウェブページのダウンロード"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# C#を使ってウェブページをダウンロードする方法

## 何となぜ？ (What & Why?)
ウェブページのダウンロードは、サーバー上のウェブページの内容を自身のデバイスにコピーすることです。これは、HTMLの解析、APIのテスト、またはオフラインで使用するためのデータ収集など、あらゆる種類のプログラミングタスクを容易にするためにプログラマーによって行われます。

## 方法 (How to:)
```C#  
using System;
using System.Net.Http;
using System.Threading.Tasks;

public class WebClientDemo
{
    private static readonly HttpClient client = new HttpClient();

    public static async Task Main()
    {
        var webpage = await client.GetStringAsync("https://www.example.com");
        Console.WriteLine(webpage);
    }
}
```
上記のコードはwww.example.comのページ内容を表示します。HttpClientの`GetStringAsync`メソッドは非同期にウェブページをダウンロードし、その結果を文字列として返します。

## ディープダイブ (Deep Dive)
####歴史的な文脈 (Historical Context)
以前には、`WebClient`クラスが広く使用されていましたが、より新しい`HttpClient`クラスは、より持続的な接続とより良いパフォーマンスを提供します。

####代替手段 (Alternatives )
もしあなたがデータをより細かく操作することが必要な場合、`GetStringAsync`の代わりに`GetStreamAsync`を使用すると、ダウンロードされたデータをストリームとして取得することができます。

####実装詳細 (Implementation Details)
`HttpClient`は`IDisposable`を実装していないため、`using`ブロックの外に置くことが推奨されます。また、アプリケーション全体で1つのインスタンスを再利用することが最善の方法です。

## 関連情報 (See Also)
- MSDNのHttpClientクラス: [https://docs.microsoft.com/ja-jp/dotnet/api/system.net.http.httpclient?view=net-5.0](https://docs.microsoft.com/ja-jp/dotnet/api/system.net.http.httpclient?view=net-5.0)
- MSDNのWebClientクラス: [https://docs.microsoft.com/ja-jp/dotnet/api/system.net.webclient?view=net-5.0](https://docs.microsoft.com/ja-jp/dotnet/api/system.net.webclient?view=net-5.0)