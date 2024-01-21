---
title:                "HTTPリクエストの送信"
date:                  2024-01-20T17:59:44.386247-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTPリクエストの送信"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
プログラマーがHTTPリクエストを送ると、ウェブサーバーとデータを交換できます。APIとの通信や情報取得のためによく使われます。

## How to: (やり方)
```Haskell
-- 'http-conduit' パッケージを利用
import Network.HTTP.Simple

-- 簡単なGETリクエストの例
main :: IO ()
main = do
    response <- httpBS "http://httpbin.org/get"
    putStrLn $ "The status code was: " ++ show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    putStrLn $ "The response body was: " ++ show (getResponseBody response)
```
実行結果のサンプル:
```
The status code was: 200
["application/json"]
The response body was: "{\"args\":{},\"headers\":{...},\"url\":\"http://httpbin.org/get\"}"
```

## Deep Dive (深く掘り下げる)
HTTPリクエストを送る機能はHaskellの始まりから重視されています。ウェブ開発やウェブサービスの中核的な部分だからです。選択肢はいくつかあります：`http-conduit`、`wreq`、`req` などが有名です。`http-conduit` はストリーミングに対応し、大きなデータも扱いやすいです。内部では、パフォーマンス重視でHTTP/1.1プロトコルを使っています。

## See Also (参照)
- [`http-conduit` パッケージ](https://www.stackage.org/package/http-conduit)
- [`wreq` パッケージ](https://www.stackage.org/package/wreq)
- [`req` パッケージ](https://www.stackage.org/package/req)