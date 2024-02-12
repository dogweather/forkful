---
title:                "基本認証を使用したHTTPリクエストの送信"
aliases: - /ja/elm/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:01:28.437893-07:00
model:                 gpt-4-1106-preview
simple_title:         "基本認証を使用したHTTPリクエストの送信"

tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/sending-an-http-request-with-basic-authentication.md"
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
