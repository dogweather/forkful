---
aliases:
- /ja/elixir/sending-an-http-request-with-basic-authentication/
date: 2024-01-20 18:01:33.326186-07:00
description: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u4FE1\u3057\u3001\u57FA\
  \u672C\u8A8D\u8A3C\u3092\u4F7F\u3046\u3053\u3068\u3067\u3001\u30BB\u30AD\u30E5\u30EA\
  \u30C6\u30A3\u3092\u5FC5\u8981\u3068\u3059\u308B\u30B5\u30FC\u30D3\u30B9\u306B\u30A2\
  \u30AF\u30BB\u30B9\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\
  \u3053\u308C\u3092\u884C\u3046\u3053\u3068\u3067\u3001\u30BB\u30AD\u30E5\u30EA\u30C6\
  \u30A3\u304C\u4FDD\u305F\u308C\u305F\u72B6\u614B\u3067\u30C7\u30FC\u30BF\u3092\u4EA4\
  \u63DB\u3067\u304D\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:54.642844
model: gpt-4-1106-preview
summary: "HTTP\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u4FE1\u3057\u3001\u57FA\u672C\
  \u8A8D\u8A3C\u3092\u4F7F\u3046\u3053\u3068\u3067\u3001\u30BB\u30AD\u30E5\u30EA\u30C6\
  \u30A3\u3092\u5FC5\u8981\u3068\u3059\u308B\u30B5\u30FC\u30D3\u30B9\u306B\u30A2\u30AF\
  \u30BB\u30B9\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3053\
  \u308C\u3092\u884C\u3046\u3053\u3068\u3067\u3001\u30BB\u30AD\u30E5\u30EA\u30C6\u30A3\
  \u304C\u4FDD\u305F\u308C\u305F\u72B6\u614B\u3067\u30C7\u30FC\u30BF\u3092\u4EA4\u63DB\
  \u3067\u304D\u307E\u3059\u3002"
title: "\u57FA\u672C\u8A8D\u8A3C\u3092\u4F7F\u7528\u3057\u305FHTTP\u30EA\u30AF\u30A8\
  \u30B9\u30C8\u306E\u9001\u4FE1"
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
