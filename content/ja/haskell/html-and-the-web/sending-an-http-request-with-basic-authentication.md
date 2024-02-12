---
title:                "基本認証を使用したHTTPリクエストの送信"
aliases: - /ja/haskell/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:02:06.558325-07:00
model:                 gpt-4-1106-preview
simple_title:         "基本認証を使用したHTTPリクエストの送信"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

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
