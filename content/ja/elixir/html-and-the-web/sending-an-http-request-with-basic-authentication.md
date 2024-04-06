---
date: 2024-01-20 18:01:33.326186-07:00
description: "How to: (\u3084\u308A\u65B9) \u3053\u306E\u30B3\u30FC\u30C9\u306F\u57FA\
  \u672C\u8A8D\u8A3C\u3067\u4FDD\u8B77\u3055\u308C\u305F\u30EA\u30BD\u30FC\u30B9\u3078\
  \u306EHTTP GET\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u4FE1\u3059\u308B\u65B9\
  \u6CD5\u3092\u793A\u3057\u3066\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.558201-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) \u3053\u306E\u30B3\u30FC\u30C9\u306F\u57FA\u672C\u8A8D\
  \u8A3C\u3067\u4FDD\u8B77\u3055\u308C\u305F\u30EA\u30BD\u30FC\u30B9\u3078\u306EHTTP\
  \ GET\u30EA\u30AF\u30A8\u30B9\u30C8\u3092\u9001\u4FE1\u3059\u308B\u65B9\u6CD5\u3092\
  \u793A\u3057\u3066\u3044\u307E\u3059\u3002"
title: "\u57FA\u672C\u8A8D\u8A3C\u3092\u4F7F\u7528\u3057\u305FHTTP\u30EA\u30AF\u30A8\
  \u30B9\u30C8\u306E\u9001\u4FE1"
weight: 45
---

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
