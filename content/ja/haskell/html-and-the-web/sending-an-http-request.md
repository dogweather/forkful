---
date: 2024-01-20 17:59:44.386247-07:00
description: "\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u304CHTTP\u30EA\u30AF\u30A8\u30B9\
  \u30C8\u3092\u9001\u308B\u3068\u3001\u30A6\u30A7\u30D6\u30B5\u30FC\u30D0\u30FC\u3068\
  \u30C7\u30FC\u30BF\u3092\u4EA4\u63DB\u3067\u304D\u307E\u3059\u3002API\u3068\u306E\
  \u901A\u4FE1\u3084\u60C5\u5831\u53D6\u5F97\u306E\u305F\u3081\u306B\u3088\u304F\u4F7F\
  \u308F\u308C\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.179660-06:00'
model: gpt-4-1106-preview
summary: "\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u304CHTTP\u30EA\u30AF\u30A8\u30B9\u30C8\
  \u3092\u9001\u308B\u3068\u3001\u30A6\u30A7\u30D6\u30B5\u30FC\u30D0\u30FC\u3068\u30C7\
  \u30FC\u30BF\u3092\u4EA4\u63DB\u3067\u304D\u307E\u3059\u3002API\u3068\u306E\u901A\
  \u4FE1\u3084\u60C5\u5831\u53D6\u5F97\u306E\u305F\u3081\u306B\u3088\u304F\u4F7F\u308F\
  \u308C\u307E\u3059\u3002."
title: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u9001\u4FE1"
weight: 44
---

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
