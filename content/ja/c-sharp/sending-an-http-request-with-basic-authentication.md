---
title:                "基本認証を用いてhttpリクエストを送信する"
html_title:           "C#: 基本認証を用いてhttpリクエストを送信する"
simple_title:         "基本認証を用いてhttpリクエストを送信する"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## なぜ
HTTPリクエストを基本認証で送信する必要があるのかを2文で説明します。

一般的に、ウェブサイトやアプリケーションでログインや個人情報の保護のために、ユーザーの認証を行う必要があります。基本認証は最も単純な認証形式であり、HTTPリクエストに含まれるユーザー名とパスワードを用いて認証を行います。この認証を使うことで、サーバー側はユーザーの正当性を確認し、セキュリティを保護することができます。

## 使い方
基本認証でHTTPリクエストを送信するためには、次のようなコードを使用します。

```csharp
// リクエストを作成
WebRequest request = WebRequest.Create("http://example.com/endpoint");

// ユーザー名とパスワードを指定
request.Credentials = new NetworkCredential("ユーザー名", "パスワード");

// リクエストを送信してレスポンスを取得
WebResponse response = request.GetResponse();

// レスポンスを処理
using(StreamReader reader = new StreamReader(response.GetResponseStream()))
{
    Console.WriteLine(reader.ReadToEnd());
}
```

この例では、```WebRequest```クラスを使用してHTTPリクエストを作成し、```Credentials```プロパティを使用してユーザー名とパスワードを指定しています。そして、```request.GetResponse()```メソッドを使用してリクエストを送信し、レスポンスを```StreamReader```を使用して処理しています。

## 深堀り
実際には、基本認証にはさまざまな種類があります。例えば、```Basic```や```Digest```などがあり、それぞれ違う仕組みを使用して認証を行います。また、セキュリティを強化するために、SSL/TLSを使用して暗号化した通信を行うこともできます。詳細については、Microsoftの公式ドキュメントを参照してください。

参考：
- [HTTP 基本認証の動作方法](https://docs.microsoft.com/ja-jp/dotnet/api/system.net.httpwebrequest.credentials?view=netframework-4.8)
- [HTTP SSL/TLS 接続の確立](https://docs.microsoft.com/ja-jp/dotnet/api/system.net.security.sslstream?view=netframework-4.8)

## 参考リンク
- [MSDN: HTTP 基本認証の動作方法](https://docs.microsoft.com/ja-jp/dotnet/api/system.net.httpwebrequest.credentials?view=netframework-4.8)
- [MSDN: HTTP SSL/TLS 接続の確立](https://docs.microsoft.com/ja-jp/dotnet/api/system.net.security.sslstream?view=netframework-4.8)
- [Sending HTTP Requests with Basic Authentication in C#](https://www.c-sharpcorner.com/article/sending-http-requests-with-basic-authentication-in-c-sharp/)