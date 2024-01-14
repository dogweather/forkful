---
title:                "Gleam: 送信中のhttpリクエスト"
simple_title:         "送信中のhttpリクエスト"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

Gleamプログラミングのブログ記事

## なぜ？

HTTPリクエストを送信することの重要性について、わずか1-2文で説明します。Gleamプログラミングを行う際に、なぜHTTPリクエストを送信する必要があるのか、その理由をご紹介いたします。

## 方法

コーディングの例と、"```Gleam ... ```"コードブロック内でのサンプルの出力を示します。これを参考に、GleamプログラミングにおけるHTTPリクエストの送信方法を実践的に学んでいきましょう。

```Gleam
// HTTPリクエストを送信するためのパッケージをインポートします
import gleam/http.{Request, Response}

// リクエストを作成し、例として"www.example.com"に送信します
let request =
  Request.get("https://www.example.com")

// レスポンスを取得します
let response = Response.send(request)

// レスポンスから状態コードを取得し、表示します
let status_code = Response.status_code(response)
io.print("Status code: " ++ String.to_int(status_code))

// レスポンスのボディを取得し、表示します
let body = Response.body(response)
io.print("Response body: " ++ body)
```

上記のコードを実行すると、例えば"Status code: 200"や"Response body: Hello World!"といった出力が得られます。

## ディープダイブ

さらにHTTPリクエストの仕組みや機能について詳しくご紹介します。リクエストヘッダーやボディをカスタマイズする方法や、より複雑な形式のリクエストを送信する方法など、より高度なテクニックも解説します。

注意: 実際にHTTPリクエストを送信する際には、セキュリティ上の理由からアクセストークンやAPIキーなどの機密情報をコード内に直接記述しないようにお気を付けください。

## おわりに

ぜひこの記事を参考にして、GleamプログラミングにおけるHTTPリクエストの送信方法をマスターしてください。より高度な処理を行うためには、HTTPリクエストの機能をしっかりと理解することが大切です。

## 関連リンク

- [Gleam公式ドキュメント: HTTPリクエストを送信する方法](https://gleam.run/articles/http-request)
- [Gleam公式ドキュメント: レスポンスを処理する方法](https://gleam.run/articles/http-response)
- [GleamのGitHubリポジトリ](https://github.com/gleam-lang/gleam)
- [Gleamフォーラム](https://gleam.discourse.group/)