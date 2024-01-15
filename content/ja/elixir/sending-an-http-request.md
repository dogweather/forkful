---
title:                "httpリクエストの送信"
html_title:           "Elixir: httpリクエストの送信"
simple_title:         "httpリクエストの送信"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## なぜ

HTTPリクエストを送信する理由はたくさんあります。最も一般的な理由は、WebサービスやAPIにアクセスするためです。また、データを取得したり送信するためにも使用されます。

## 使い方

基本的なHTTPリクエスト送信の方法は、`HTTPoison`ライブラリを使用することです。まず、プロジェクトの`mix.exs`ファイルに`HTTPoison`を追加します。

```
def deps do
  [
    {:httpoison, "~> 1.6.0"}
  ]
end
```

次に、`mix deps.get`コマンドを実行して依存関係を更新します。

次に、リクエストを送信するための簡単な例を見てみましょう。例として、`https://api.github.com/users/elixir-lang`からユーザー情報を取得することを考えます。

```
# HTTPoisonライブラリを使用する
HTTPoison.get("https://api.github.com/users/elixir-lang")

# 出力例:
# {:ok,
#  %HTTPoison.Response{body: "{\"login\":\"elixir-lang\", ... }",
#   headers: [{"server", ...}, ...], status_code: 200}}
```

この例では、`HTTPoison.get()`関数を使用して、指定したURLにGETリクエストを送信しています。成功した場合、レスポンスは{:ok, response}の形式で返されます。`response`オブジェクトには、リクエストで取得したボディ、ヘッダー、ステータスコードなどが含まれています。

## 深堀り

HTTPリクエストを送信する場合、`HTTPoison`ライブラリの他にもいくつかのオプションがあります。例えば、`HTTPotion`や`Mint`などのライブラリも使用することができます。

また、`HTTPoison`は非同期リクエストをサポートしているため、複数のリクエストを同時に送信することも可能です。さらに、リクエストのヘッダーをカスタマイズしたり、トークン認証やベーシック認証などの認証方法を使用したりすることもできます。

## 参考リンク

- [HTTPoisonドキュメント](https://hexdocs.pm/httpoison/1.6.0/)
- [ElixirでHTTPリクエストを送信](https://elixirschool.com/jp/lessons/specifics/http/)
- [HTTPリクエストとは](https://developer.mozilla.org/ja/docs/Web/HTTP/Basics_of_HTTP/Overview)