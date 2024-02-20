---
date: 2024-01-20 18:01:28.437893-07:00
description: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3067\u30D9\u30FC\u30B7\u30C3\u30AF\
  \u8A8D\u8A3C\u3092\u9001\u308B\u3068\u306F\u3001\u30E6\u30FC\u30B6\u30FC\u540D\u3068\
  \u30D1\u30B9\u30EF\u30FC\u30C9\u3092\u30A8\u30F3\u30B3\u30FC\u30C9\u3057\u3066\u30B5\
  \u30FC\u30D0\u30FC\u306B\u30A2\u30AF\u30BB\u30B9\u3059\u308B\u3053\u3068\u3067\u3059\
  \u3002\u3053\u308C\u3092\u884C\u3046\u7406\u7531\u306F\u3001\u30EA\u30BD\u30FC\u30B9\
  \u3078\u306E\u5B89\u5168\u306A\u30A2\u30AF\u30BB\u30B9\u3092\u78BA\u4FDD\u3059\u308B\
  \u305F\u3081\u3067\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:01.165114
model: gpt-4-1106-preview
summary: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3067\u30D9\u30FC\u30B7\u30C3\u30AF\u8A8D\
  \u8A3C\u3092\u9001\u308B\u3068\u306F\u3001\u30E6\u30FC\u30B6\u30FC\u540D\u3068\u30D1\
  \u30B9\u30EF\u30FC\u30C9\u3092\u30A8\u30F3\u30B3\u30FC\u30C9\u3057\u3066\u30B5\u30FC\
  \u30D0\u30FC\u306B\u30A2\u30AF\u30BB\u30B9\u3059\u308B\u3053\u3068\u3067\u3059\u3002\
  \u3053\u308C\u3092\u884C\u3046\u7406\u7531\u306F\u3001\u30EA\u30BD\u30FC\u30B9\u3078\
  \u306E\u5B89\u5168\u306A\u30A2\u30AF\u30BB\u30B9\u3092\u78BA\u4FDD\u3059\u308B\u305F\
  \u3081\u3067\u3059\u3002"
title: "\u57FA\u672C\u8A8D\u8A3C\u3092\u4F7F\u7528\u3057\u305FHTTP\u30EA\u30AF\u30A8\
  \u30B9\u30C8\u306E\u9001\u4FE1"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
HTTPリクエストでベーシック認証を送るとは、ユーザー名とパスワードをエンコードしてサーバーにアクセスすることです。これを行う理由は、リソースへの安全なアクセスを確保するためです。

## How to: (やり方)
Elmでは、ベーシック認証を伴うHTTPリクエストを送るには、カスタムヘッダーを作成して、リクエストに付加します。次のコード例をご覧ください。

```Elm
import Http
import Base64

type alias Credentials =
    { username : String
    , password : String
    }

createBasicAuthHeader : Credentials -> Http.Header
createBasicAuthHeader creds =
    let
        encodedCredentials =
            Base64.encode (creds.username ++ ":" ++ creds.password)
    in
    Http.header "Authorization" ("Basic " ++ encodedCredentials)

-- Send a request with basic authentication
sendRequest : String -> Credentials -> Cmd msg
sendRequest url creds =
    let
        headers = 
            [ createBasicAuthHeader creds ]
    in
    Http.request
        { method = "GET"
        , headers = headers
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectWhatever msg
        , timeout = Nothing
        , tracker = Nothing
        }

-- Example usage
login : Cmd msg
login =
    sendRequest "https://your-api.com/protected" { username = "user1", password = "pass123" }
```

正常に認証されると、期待されるデータが得られるでしょう。それ以外の場合は、認証エラーのレスポンスが返されます。

## Deep Dive (詳細情報)
ベーシック認証の歴史は古く、初期のインターネットで使われ始めました。現在では、よりセキュアな方法（例えばOAuth 2.0）が好まれますが、単純なAPIや内部ツールでは依然として使われています。Elmでの実装では、Base64でエンコードする関数を使って認証情報をエンコードします。注意点として、HTTPS経由で送信することが重要です。これによって、中間者攻撃を防ぐことができます。Elmの`Http`モジュールのバージョン2.0.0には、リクエストを細かく制御するための機能が充実しています。

## See Also (関連情報)
- Elm Http package documentation: [https://package.elm-lang.org/packages/elm/http/latest/](https://package.elm-lang.org/packages/elm/http/latest/)
- Base64 encoding in Elm: [https://package.elm-lang.org/packages/truqu/elm-base64/latest/](https://package.elm-lang.org/packages/truqu/elm-base64/latest/)
- Elm language introduction: [https://elm-lang.org/](https://elm-lang.org/)
- HTTP Basic Authentication specification: [https://tools.ietf.org/html/rfc7617](https://tools.ietf.org/html/rfc7617)
