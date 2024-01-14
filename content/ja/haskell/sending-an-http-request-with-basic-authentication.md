---
title:                "Haskell: 基本認証を使用したhttpリクエストの送信"
simple_title:         "基本認証を使用したhttpリクエストの送信"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## なぜ

HTTPリクエストを基本認証で送信する理由は何でしょうか？基本認証はウェブアプリケーションやAPIへのアクセスを制限するために使用されます。サーバーが誰がリクエストを送信しているかを確認するために、ユーザー名とパスワードを使用します。

## 使い方

基本認証を使用してHTTPリクエストを送信する方法を見ていきましょう。まずは、以下のコードブロックを使用してHTTPリクエストを作成します。

```Haskell
{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Req

main :: IO ()
main = do
  let opts = basicAuth "username" "password"
  req <- req GET
          (https "example.com" /: "api" /: "users")
          NoReqBody
          jsonResponse
          opts
  response <- runReq defaultHttpConfig req
  print (responseBody response :: Value)
```

このコードでは、ユーザー名とパスワードを使用してリクエストオプションを作成し、そのオプションを使用してリクエストを送信します。そして、レスポンスを取得して出力します。

## 深堀り

基本認証を使用してHTTPリクエストを送信する際、`basicAuth`関数の他にもオプションの設定やエラーハンドリングなど、さまざまなことに気をつける必要があります。また、HTTPリクエストのヘッダーに認証情報を追加する方法や、接続を安全に保つためのセキュリティプロトコルの使用についても理解する必要があります。

## その他の参考資料

- [HaskellでのHTTPリクエストの送信方法](https://devlog.hexrabbit.jp/sending-http-requests-with-haskell)
- [サーバーサイドHaskell入門 - HTTPリクエストの送信](http://nippondanji.blogspot.com/2019/08/introduction-to-server-side-haskell-sending-http-requests.html)
- [基本認証についての詳細な説明](https://qiita.com/kakokeigo/items/06c4b84f34a33fa4e207)
- [Haskellでの基本認証の仕組み](https://www.karaku.net/magazine/2016/04/19/haskell%E3%81%A7%E3%81%AE%E3%83%87%E3%83%BC%E3%82%BF%E5%8C%96%E5%9E%8B%E3%81%AB%E3%82%88%E3%82%8Bhttp%E9%80%9A%E4%BF%A1%E3%81%AE%E5%AE%9F%E8%A3%85part1/)