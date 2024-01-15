---
title:                "基本認証を使用してhttpリクエストを送信する"
html_title:           "Gleam: 基本認証を使用してhttpリクエストを送信する"
simple_title:         "基本認証を使用してhttpリクエストを送信する"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# なぜ
## なぜHTTPリクエストに基本認証を使用するのか？
基本認証を使用することで、ウェブサイトやアプリケーションにログインする必要があるユーザー認証を簡単に行うことができます。基本認証は安全で簡単な方法ですが、Gleamを使用することでより柔軟にコーディングできます。

# 方法
## Gleamで基本認証を使用したHTTPリクエストを送信する方法
基本認証を使ったHTTPリクエストを送信するには、まず`gleam/http`モジュールをインポートします。次に、送信したいURLを指定し、`BasicAuth`タイプのクレデンシャルを含む`headers`を定義します。最後に、`http.send`関数を使用してリクエストを送信します。以下の例を参考にしてください。

```
Gleam バージョン
import gleam/http

pub fn main() {
  // URLの指定
  let url = "https://example.com/login";

  // BasicAuth認証を使うためのヘッダーの定義
  let auth_header = {
    let user = "username";
    let pass = "password";

    ("Authorization", "Basic " ++ Base.encode_to_string(user ++ ":" ++ pass))
  };

  // HTTPリクエストを送信
  let response = http.send(url, {
    headers: [auth_header]
  });

  // リクエストを出力
  case response {
    Ok(resp) -> {
      resp.body
      |> Bytes.to_string
      |> String.replace("\n", "")
      |> String.replace("\t", "")
      |> String.replace(" ", "")
    }
    Err(_) -> "リクエストの送信に失敗しました。"
  }
}
```

以下のような結果が返されるはずです。

```
"Welcome! You are now logged in as username."
```

# 深堀り
## 基本認証を使用したHTTPリクエストの詳細
基本認証は、希望するリソースに対してユーザー名とパスワードを構成することで、簡単な認証を提供します。この認証方法では、パスワードは平文で送信されるため、HTTPSと組み合わせて使用することが推奨されています。しかし、Gleamを使えば安全な方法で認証に関する情報を扱うことができるため、セキュリティ上の懸念は軽減されます。

＃ See Also
- [HTTP Basic認証について| MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [Gleam httpモジュールドキュメント]（https://gleam.run/packages/gleam/http/latest/）