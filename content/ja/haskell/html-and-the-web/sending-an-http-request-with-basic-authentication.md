---
date: 2024-01-20 18:02:06.558325-07:00
description: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306B\u30D9\u30FC\u30B7\u30C3\u30AF\
  \u8A8D\u8A3C\u3092\u4F7F\u3044\u307E\u3059\u3002\u3053\u308C\u306F\u30A6\u30A7\u30D6\
  \u30EA\u30BD\u30FC\u30B9\u3078\u306E\u5B89\u5168\u306A\u30A2\u30AF\u30BB\u30B9\u3092\
  \u63D0\u4F9B\u3059\u308B\u305F\u3081\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u306F\u3001\u8A8D\u8A3C\u304C\u5FC5\u8981\u306A\u30EA\u30BD\u30FC\u30B9\u306B\u30A2\
  \u30AF\u30BB\u30B9\u3059\u308B\u306B\u306F\u3001\u30E6\u30FC\u30B6\u30FC\u540D\u3068\
  \u30D1\u30B9\u30EF\u30FC\u30C9\u3092\u4F7F\u3046\u5FC5\u8981\u304C\u3042\u308A\u307E\
  \u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.183521-06:00'
model: gpt-4-1106-preview
summary: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306B\u30D9\u30FC\u30B7\u30C3\u30AF\u8A8D\
  \u8A3C\u3092\u4F7F\u3044\u307E\u3059\u3002\u3053\u308C\u306F\u30A6\u30A7\u30D6\u30EA\
  \u30BD\u30FC\u30B9\u3078\u306E\u5B89\u5168\u306A\u30A2\u30AF\u30BB\u30B9\u3092\u63D0\
  \u4F9B\u3059\u308B\u305F\u3081\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u306F\
  \u3001\u8A8D\u8A3C\u304C\u5FC5\u8981\u306A\u30EA\u30BD\u30FC\u30B9\u306B\u30A2\u30AF\
  \u30BB\u30B9\u3059\u308B\u306B\u306F\u3001\u30E6\u30FC\u30B6\u30FC\u540D\u3068\u30D1\
  \u30B9\u30EF\u30FC\u30C9\u3092\u4F7F\u3046\u5FC5\u8981\u304C\u3042\u308A\u307E\u3059\
  \u3002."
title: "\u57FA\u672C\u8A8D\u8A3C\u3092\u4F7F\u7528\u3057\u305FHTTP\u30EA\u30AF\u30A8\
  \u30B9\u30C8\u306E\u9001\u4FE1"
weight: 45
---

## What & Why? (何となぜ？)
HTTPリクエストにベーシック認証を使います。これはウェブリソースへの安全なアクセスを提供するためです。プログラマは、認証が必要なリソースにアクセスするには、ユーザー名とパスワードを使う必要があります。

## How to (実装方法):
HaskellでHTTPリクエストにベーシック認証を加える例です：

```Haskell
import Network.HTTP.Simple
import Data.ByteString.Char8 (pack)

main :: IO ()
main = do
  let auth = "Basic " ++ (unpack . Base64.encode . pack) "username:password"
  request' <- parseRequest "GET http://example.com"
  let request = setRequestHeader "Authorization" [pack auth] request'
  response <- httpLBS request
  putStrLn $ "Status code: " ++ show (getResponseStatusCode response)
```

サンプル出力:

```
Status code: 200
```

## Deep Dive (掘り下げ):
### 歴史的背景:
ベーシック認証は古いが信頼できる手法です。最初のHTTP/1.0提案で使用されました。ただし、平文でのユーザー名とパスワードの送信は安全ではないため、HTTPSの使用が推奨されます。

### 代替手段:
オプションには、OAuthやTokenベースの認証があります。これらはより安全で、現代のAPIにおいて好まれます。

### 実装詳細:
`Network.HTTP.Simple`モジュールは、リクエストヘッダを設定しやすくします。`Authorization`ヘッダに`Basic`認証トークンを挿入することで認証情報が送信されます。パスワードとユーザー名はBase64でエンコードする必要があります。

## See Also (関連情報):
- Haskell `http-conduit`: https://hackage.haskell.org/package/http-conduit
- HTTP 認証: https://developer.mozilla.org/ja/docs/Web/HTTP/Authentication
- Base64 エンコーディング: https://hackage.haskell.org/package/base64-bytestring
