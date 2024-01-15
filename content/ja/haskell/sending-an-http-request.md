---
title:                "「HTTPリクエストの送信」"
html_title:           "Haskell: 「HTTPリクエストの送信」"
simple_title:         "「HTTPリクエストの送信」"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## なぜ
なぜ人々がHTTPリクエストを送ることに取り組むのか、主な理由は2つあります。まず第一に、インターネットを介して情報をやりとりする上で、HTTPは重要なプロトコルであるということです。そして第二に、Haskellは強い静的型付け言語であり、HTTPリクエストを送る際にも安全性を確保できるということです。

## 使い方
```Haskell
import Network.HTTP

main = do
  request <- simpleHTTP (getRequest "https://example.com")
  body <- getResponseBody request
  print body
```
サンプルコードを実行すると、HTTPSプロトコルを使用してexample.comから取得したHTMLの内容が表示されます。

## 深堀り
HTTPリクエストを送信するためにHaskellを使用するには、まずNetwork.HTTPモジュールをインポートする必要があります。その後、simpleHTTP関数を使用してリクエストを作成し、getRequest関数を使用してリクエストのURLを指定します。そして、getResponseBody関数を使うことでレスポンスボディを取得することができます。Haskellは強力なパターンマッチング機能を備えているため、受け取ったレスポンスを適切に処理することができます。

## 参考リンク
- [Network.HTTPモジュールのドキュメント](https://hackage.haskell.org/package/HTTP)
- [HaskellでHTTPリクエストを送信する方法](https://stackoverflow.com/questions/16210548/sending-a-http-request-in-haskell)
- [HaskellとHTTPクライアントライブラリの比較](https://www.fpcomplete.com/blog/2017/10/comparing-haskell-web-libraries)