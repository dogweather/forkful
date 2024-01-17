---
title:                "HTTPリクエストの送信"
html_title:           "C#: HTTPリクエストの送信"
simple_title:         "HTTPリクエストの送信"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/sending-an-http-request.md"
---

{{< edit_this_page >}}

「あなたがWeb開発やAPIに取り組んでいるなら、HTTPリクエストとは何か、そしてそれを送信する理由は何かを知っておくことが重要です。この記事では、わかりやすく説明するので、最後まで読んでください！」

## What & Why?
HTTPリクエストを送信するとは、Webサーバーから情報を要求することです。例えば、あなたがSNSサイトにログインする際は、HTTPリクエストを送信して、アカウント情報をサーバーから取得します。プログラマーがこれを行う理由は、ユーザーからの要求に応じて適切なデータを提供するためです。

## How to:
```C#
// HTTPリクエストを送信する例
using System;
using System.Net.Http;

async static void SendRequest(){
    HttpClient client = new HttpClient();
    var response = await client.GetAsync("https://www.example.com/");
    string result = await response.Content.ReadAsStringAsync();
    Console.WriteLine(result);
}
```
上記の例では、HttpClientクラスを使用して、Webサーバーにリクエストを送信しています。最初に、サーバーのURLを指定し、その後、GetAsync()メソッドを使用してリクエストを送信し、レスポンスを待ちます。最後に、レスポンスの内容を読み取り、コンソールに表示しています。

## Deep Dive:
HTTPリクエストは、HTTPプロトコルを使用してデータを送信するための方式の一つです。現在では、Webサーバーとブラウザ間の通信に最もよく使用されています。しかし、過去にはSOAPやXML-RPCなどの異なるHTTPベースのプロトコルが使用されていました。また、現在では、JavaScriptを使用してリクエストを送信することも可能です。

## See Also:
- [HTTPリクエストとは？](https://developer.mozilla.org/ja/docs/Web/HTTP/Overview)
- [C# による HTTP リクエストの送信と応答の処理](https://docs.microsoft.com/ja-jp/dotnet/framework/network-programming/sending-data-using-the-httpclient-class)
- [HTTP リクエストの発行](https://www.ibm.com/support/knowledgecenter/SSWQ2H_4.0.0/admin-system-admin/channelpropertiestype.html)
- [JavaScript による HTTP リクエストの送信](https://developer.mozilla.org/ja/docs/Web/API/Fetch_API/Using_Fetch)