---
date: 2024-01-20 18:01:28.437893-07:00
description: "How to: (\u3084\u308A\u65B9) Elm\u3067\u306F\u3001\u30D9\u30FC\u30B7\
  \u30C3\u30AF\u8A8D\u8A3C\u3092\u4F34\u3046HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\
  \u9001\u308B\u306B\u306F\u3001\u30AB\u30B9\u30BF\u30E0\u30D8\u30C3\u30C0\u30FC\u3092\
  \u4F5C\u6210\u3057\u3066\u3001\u30EA\u30AF\u30A8\u30B9\u30C8\u306B\u4ED8\u52A0\u3057\
  \u307E\u3059\u3002\u6B21\u306E\u30B3\u30FC\u30C9\u4F8B\u3092\u3054\u89A7\u304F\u3060\
  \u3055\u3044\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.007427-06:00'
model: gpt-4-1106-preview
summary: "Elm\u3067\u306F\u3001\u30D9\u30FC\u30B7\u30C3\u30AF\u8A8D\u8A3C\u3092\u4F34\
  \u3046HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u308B\u306B\u306F\u3001\u30AB\
  \u30B9\u30BF\u30E0\u30D8\u30C3\u30C0\u30FC\u3092\u4F5C\u6210\u3057\u3066\u3001\u30EA\
  \u30AF\u30A8\u30B9\u30C8\u306B\u4ED8\u52A0\u3057\u307E\u3059\u3002\u6B21\u306E\u30B3\
  \u30FC\u30C9\u4F8B\u3092\u3054\u89A7\u304F\u3060\u3055\u3044."
title: "\u57FA\u672C\u8A8D\u8A3C\u3092\u4F7F\u7528\u3057\u305FHTTP\u30EA\u30AF\u30A8\
  \u30B9\u30C8\u306E\u9001\u4FE1"
weight: 45
---

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
