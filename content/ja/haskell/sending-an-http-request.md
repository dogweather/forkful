---
title:                "Haskell: 「HTTPリクエストの送信」"
simple_title:         "「HTTPリクエストの送信」"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## なぜ

HaskellでHTTPリクエストを送信することの魅力について説明します。

## 方法

### 基本的なHTTPリクエストの送信方法
```Haskell
import Network.HTTP.Simple

main = do
    response <- httpLBS "http://example.com"
    print $ getResponseStatusCode response
```
上記のコードは、Network.HTTP.Simpleモジュールを使用してHTTPリクエストを送信し、レスポンスのステータスコードを表示するものです。

### カスタムヘッダーを含めたHTTPリクエストの送信
```Haskell
import Network.HTTP.Simple

main = do
    let request = setRequestHeader "Content-Type" ["application/json"]
                $ setRequestMethod "POST"
                $ setRequestBodyLBS "{\"key\": \"value\"}"
                $ setRequestHeader "Authorization" ["Bearer token"]
                $ "http://example.com"
    response <- httpLBS request
    putStrLn $ "Response body: " ++ (getResponseBody response)
```
上記のコードでは、カスタムのヘッダーを設定してPOSTリクエストを送信し、レスポンスのボディを表示しています。"Content-Type"のヘッダーでは、JSON形式のデータを送信しています。さらに、"Authorization"のヘッダーではベアラートークンを使用して認証を行っています。

## 深堀り

Network.HTTP.Simpleモジュールでは、HTTPリクエストを送信する際によく使用する関数がたくさん用意されています。例えば、`setRequestHeader`関数を使用してヘッダーを設定したり、`setRequestBody`関数を使用してボディを設定したりすることができます。さらに詳しくは、公式ドキュメントを参照することをおすすめします。

## 参考リンク

- [Haskellによる開発フローのアーキテクチャについて](https://www.oreilly.co.jp/pub/9784873118705) 
- [HaskellでHTTP通信をする方法](https://qiita.com/suin/items/27c88da7ae1cf45760e2)