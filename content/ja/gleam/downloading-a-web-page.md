---
title:                "ウェブページのダウンロード"
date:                  2024-01-20T17:44:24.606713-07:00
model:                 gpt-4-1106-preview
simple_title:         "ウェブページのダウンロード"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?
ウェブページのダウンロードって何？ プログラマがそれをなぜやるの？
Webページのダウンロードとは、インターネット上に存在するページの内容を取得するプロセスです。プログラマは自動で情報を収集したり、サービスの統合、テスト、解析を行うためにこれを実行します。

## How to:
```gleam
import gleam/http
import gleam/httpc

pub fn download_page() {
  let result = httpc.send(http.Request(
    method: http.Get,
    url: "https://example.com",
  ))
  
  case result {
    Ok(response) -> 
      io.println(response.body) // ページの内容を表示
    Error(error) ->
      io.println(error)         // エラー内容を表示
  }
}
```

出力サンプル：
```
<html>
  <head>
    <title>Example Domain</title>
  </head>
  <body>
    <p>This domain is for use in illustrative examples in documents.</p>
  </body>
</html>
```

## Deep Dive
インターネットの初期には、コマンドラインツールやブラウザで手動でウェブページをダウンロードするのが一般的でした。だが、情報が爆発的に増える中、自動化の必要性が高まったのです。他の言語では`curl`や`wget`のようなツールがよく使われますが、Gleamは静的型付け＆Erlangの強力なバックエンドにより、並行処理や分散システムにおいて強みを発揮します。具体的には、`gleam/httpc` モジュールを活用して、HTTPリクエストを送信し、レスポンスを処理することができます。エラーハンドリングもしっかり行うことで、より堅牢なアプリケーション作成が可能です。

## See Also
- [Gleam's HTTP docs](https://hexdocs.pm/gleam_http/)
- [Erlang's httpc module documentation](http://erlang.org/doc/man/httpc.html)
- [The HTTP client request RFC](https://tools.ietf.org/html/rfc7231#section-4.3.1)