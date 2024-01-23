---
title:                "基本認証を使用したHTTPリクエストの送信"
date:                  2024-01-20T18:01:33.326186-07:00
model:                 gpt-4-1106-preview
simple_title:         "基本認証を使用したHTTPリクエストの送信"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
HTTPリクエストを送信し、基本認証を使うことで、セキュリティを必要とするサービスにアクセスします。プログラマーはこれを行うことで、セキュリティが保たれた状態でデータを交換できます。

## How to: (やり方)
```Elixir
# HTTPoisonを使った基本認証付きHTTPリクエストの例
defmodule SimpleHTTPClient do
  def send_request do
    # ベーシック認証の情報をエンコード
    auth = :base64.encode("user:password")
    
    # HTTPoisonを用いてGETリクエスト
    HTTPoison.get("https://example.com", [{"Authorization", "Basic " <> auth}])
  end
end

# サンプルの出力表示
case SimpleHTTPClient.send_request() do
  {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
    IO.puts("Success: #{body}")
  {:ok, %HTTPoison.Response{status_code: 401}} ->
    IO.puts("Unauthorized: Check your credentials.")
  {:error, %HTTPoison.Error{reason: reason}} ->
    IO.puts("Error: #{reason}")
end
```
このコードは基本認証で保護されたリソースへのHTTP GETリクエストを送信する方法を示しています。

## Deep Dive (深掘り)
基本認証は、HTTPプロトコルで定義された最も古い認証形式の一つです。ユーザー名とパスワードをBase64でエンコードし、`Authorization`ヘッダに含めます。セキュリティが厳格な状況では、代わりにOAuthやTokenベースの認証が用いられることがあります。ElixirでHTTPリクエストを送るライブラリとしては、HTTPoisonの他にもTeslaやHackneyなどがあります。基本認証はHTTPS経由でのみ使用するべきで、HTTP上ではセキュリティが確保できません。Elixirでは、`:base64`モジュールを使うと容易にエンコード処理が可能です。

## See Also (関連情報)
- [Elixir HTTPoison GitHub repository](https://github.com/edgurgel/httpoison)
- [Erlang :base64 module documentation](http://erlang.org/doc/man/base64.html)
- [MDN Web Docs HTTP Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [HTTPoison Hex Docs](https://hexdocs.pm/httpoison/HTTPoison.html)
