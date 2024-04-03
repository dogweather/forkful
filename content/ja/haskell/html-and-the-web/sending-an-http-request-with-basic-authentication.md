---
date: 2024-01-20 18:02:06.558325-07:00
description: "How to (\u5B9F\u88C5\u65B9\u6CD5): Haskell\u3067HTTP\u30EA\u30AF\u30A8\
  \u30B9\u30C8\u306B\u30D9\u30FC\u30B7\u30C3\u30AF\u8A8D\u8A3C\u3092\u52A0\u3048\u308B\
  \u4F8B\u3067\u3059\uFF1A."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.183521-06:00'
model: gpt-4-1106-preview
summary: "Haskell\u3067HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u306B\u30D9\u30FC\u30B7\u30C3\
  \u30AF\u8A8D\u8A3C\u3092\u52A0\u3048\u308B\u4F8B\u3067\u3059\uFF1A."
title: "\u57FA\u672C\u8A8D\u8A3C\u3092\u4F7F\u7528\u3057\u305FHTTP\u30EA\u30AF\u30A8\
  \u30B9\u30C8\u306E\u9001\u4FE1"
weight: 45
---

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
