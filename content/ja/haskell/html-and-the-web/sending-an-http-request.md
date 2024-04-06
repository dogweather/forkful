---
date: 2024-01-20 17:59:44.386247-07:00
description: "How to: (\u3084\u308A\u65B9) HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\
  \u9001\u308B\u6A5F\u80FD\u306FHaskell\u306E\u59CB\u307E\u308A\u304B\u3089\u91CD\u8996\
  \u3055\u308C\u3066\u3044\u307E\u3059\u3002\u30A6\u30A7\u30D6\u958B\u767A\u3084\u30A6\
  \u30A7\u30D6\u30B5\u30FC\u30D3\u30B9\u306E\u4E2D\u6838\u7684\u306A\u90E8\u5206\u3060\
  \u304B\u3089\u3067\u3059\u3002\u9078\u629E\u80A2\u306F\u3044\u304F\u3064\u304B\u3042\
  \u308A\u307E\u3059\uFF1A`http-conduit`\u3001`wreq`\u3001`req` \u306A\u3069\u304C\
  \u6709\u540D\u3067\u3059\u3002`http-conduit`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:50:56.104768-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u308B\
  \u6A5F\u80FD\u306FHaskell\u306E\u59CB\u307E\u308A\u304B\u3089\u91CD\u8996\u3055\u308C\
  \u3066\u3044\u307E\u3059\u3002\u30A6\u30A7\u30D6\u958B\u767A\u3084\u30A6\u30A7\u30D6\
  \u30B5\u30FC\u30D3\u30B9\u306E\u4E2D\u6838\u7684\u306A\u90E8\u5206\u3060\u304B\u3089\
  \u3067\u3059\u3002\u9078\u629E\u80A2\u306F\u3044\u304F\u3064\u304B\u3042\u308A\u307E\
  \u3059\uFF1A`http-conduit`\u3001`wreq`\u3001`req` \u306A\u3069\u304C\u6709\u540D\
  \u3067\u3059\u3002`http-conduit` \u306F\u30B9\u30C8\u30EA\u30FC\u30DF\u30F3\u30B0\
  \u306B\u5BFE\u5FDC\u3057\u3001\u5927\u304D\u306A\u30C7\u30FC\u30BF\u3082\u6271\u3044\
  \u3084\u3059\u3044\u3067\u3059\u3002\u5185\u90E8\u3067\u306F\u3001\u30D1\u30D5\u30A9\
  \u30FC\u30DE\u30F3\u30B9\u91CD\u8996\u3067HTTP/1.1\u30D7\u30ED\u30C8\u30B3\u30EB\
  \u3092\u4F7F\u3063\u3066\u3044\u307E\u3059\u3002"
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
