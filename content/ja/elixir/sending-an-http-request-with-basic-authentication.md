---
title:                "基本認証でHTTPリクエストを送信する"
html_title:           "Elixir: 基本認証でHTTPリクエストを送信する"
simple_title:         "基本認証でHTTPリクエストを送信する"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# なぜ？

HTTPリクエストに基本認証を使って送信する理由は、ウェブサービスの認証を行うためです。これはデータを安全に送信するために重要です。

# 使い方

```elixir
alias HTTPoison.Response
HTTPoison.start

# URLと認証情報を設定
url = "https://example.com"
auth = {"username", "password"}

# リクエストを送信
response = HTTPoison.get(url, [], basic_auth: auth)

# レスポンスのステータスコードを確認
if response.status_code == 200 do
  IO.puts "リクエストが成功しました。"
else
  IO.puts "リクエストが失敗しました。"
end

# レスポンスのボディを表示
IO.puts response.body
```

出力例：
```
リクエストが成功しました。
<!DOCTYPE html>
<html>
<head>
  <title>Welcome to Example.com</title>
</head>
<body>
  <h1>Hello World!</h1>
</body>
</html>
```

# 詳細を深く掘り下げる

基本認証は、HTTPリクエストのヘッダー内にusernameとpasswordを含めることで、サーバーがリクエストを認証できるようにします。これにより、機密性の高いデータのやりとりなど、セキュリティが必要な環境で使用されます。

# 他にも参考になる情報

- [ElixirでのHTTPリクエストを扱う方法](https://hexdocs.pm/httpoison/HTTPoison.html)
- [基本認証について詳しく学ぶ](https://developer.mozilla.org/ja/docs/Web/HTTP/Authentication)
- [Elixirの基本文法を学ぶ](https://elixir-lang.org/getting-started/introduction.html)