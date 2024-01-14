---
title:                "Gleam: 基本認証付きのhttpリクエストの送信"
simple_title:         "基本認証付きのhttpリクエストの送信"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## なぜ
HTTPリクエストを基本認証で送信するのに参加する理由は、ウェブアプリケーションを実装する上で重要な一部であるためです。特定のユーザーの認証やデータの保護を確保するために必要な手段として、基本認証は非常に便利です。

## 方法
```Gleam
import gleam/http
import gleam/bit_builder
import gleam/csv

let request =
  http
    .get("https://example.com/api/users")
    .auth("username", "password")

let response = http.request(request)

case response {
  Ok(http.Response.Simple(body)) ->
    let csv = csv.from_binary(body)

    csv
      |> csv.find("id", "name")
      |> Enum.map(fn { id, name } -> "ID: #{id}, Name: #{name}" end)
      |> Enum.join("\n")
      |> bit_builder.string()

  Error(error) ->
    error
    |> http.error_message()
    |> bit_builder.string()
}
```

この例では、Gleamの`http`モジュールを使用して、基本認証で保護されたAPIエンドポイントにHTTP GETリクエストを送信しています。レスポンスはCSV形式のリストとして取得され、IDと名前のフィールドを含む行のみがリストされます。また、基本認証に失敗した場合、エラーの詳細情報が文字列として取得されます。

## 深堀り
基本認証を使用すると、HTTPリクエストのヘッダーにユーザー名とパスワードが含まれるため、リクエストを受け取ったサーバーは送信元を認証することができます。これにより、特定のユーザーしかアクセスできないAPIエンドポイントやウェブページを作成することができます。また、SSLなどのより安全な通信プロトコルと併用することで、更にセキュリティを強化することができます。

## See Also
- [Gleamの公式ドキュメント](https://gleam.run/documentation)
- [基本認証についての詳細な説明](https://developer.mozilla.org/ja/docs/Web/HTTP/Authentication)
- [GleamでAPIクライアントを作成する方法](https://gleam.run/articles/making-an-api-client.html)