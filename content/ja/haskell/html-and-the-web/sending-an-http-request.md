---
date: 2024-01-20 17:59:44.386247-07:00
description: "How to: (\u3084\u308A\u65B9) \u5B9F\u884C\u7D50\u679C\u306E\u30B5\u30F3\
  \u30D7\u30EB."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.046676-06:00'
model: gpt-4-1106-preview
summary: ''
title: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306E\u9001\u4FE1"
weight: 44
---

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
