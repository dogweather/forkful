---
title:                "ウェブページのダウンロード"
html_title:           "Rust: ウェブページのダウンロード"
simple_title:         "ウェブページのダウンロード"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## なぜ

ウェブページをダウンロードする理由はたくさんあります。例えば、特定の情報を手元に保存したい、オフライン環境で閲覧したい、またはプログラミングの演習として使用したいなどが挙げられます。この記事では、Rust言語でウェブページをダウンロードする方法を紹介します。

## 方法

Rustでウェブページをダウンロードするには、まずは必要なクレートをインポートします。例えば、[`reqwest`](https://docs.rs/reqwest/latest/reqwest/)クレートを使うと簡単にウェブページをダウンロードできます。

```Rust
use reqwest::Error; // エラー処理を行うためのクレートをインポート
use std::fs::File; // ファイル操作をするためのクレートをインポート
use std::io::copy; // ファイルをコピーするためのクレートをインポート

fn main() -> Result<(), Error>{
  let response = reqwest::get("https://example.com")?; // ウェブページをダウンロードする
  let mut output = File::create("index.html")?; // ファイルを作成する
  copy(&mut response, &mut output)?; // ファイルをコピーする

  println!("ページをダウンロードしました！");
  Ok(()) // 処理が成功した場合には、エラーオブジェクトを返さずに`()`を返す
}
```

上の例では、[`get`](https://docs.rs/reqwest/latest/reqwest/struct.Response.html#method.text)メソッドを使用してウェブページをダウンロードし、[`copy`](https://doc.rust-lang.org/std/fs/fn.copy.html)関数を使用してファイルをコピーしています。ダウンロードしたウェブページは`index.html`という名前で保存されます。この方法を応用すれば、ダウンロードしたファイルをパースしたり、ヘッダー情報を取得したりすることもできます。

## ディープダイブ

ウェブページをダウンロードする際には、HTTPリクエストやレスポンスについても知っておく必要があります。[`reqwest`](https://docs.rs/reqwest/latest/reqwest/)クレートでは、[`Request`](https://docs.rs/reqwest/latest/reqwest/struct.Request.html)と[`Response`](https://docs.rs/reqwest/latest/reqwest/struct.Response.html)という構造体を使用してリクエストとレスポンスを処理することができます。また、HTTPリクエスト時には[`Client`](https://docs.rs/reqwest/latest/reqwest/struct.Client.html)構造体を使用して、セッションを作成することができます。

さらに、ウェブページをダウンロードする際にはHTTPSを使用することが推奨されています。[`reqwest`](https://docs.rs/reqwest/latest/reqwest/)クレートでは、[`ClientBuilder`](https://docs.rs/reqwest/latest/reqwest/struct.ClientBuilder.html)を使用してHTTPSエンドポイントを検証することもできます。

## もっと詳しく知りたい

- [`reqwest`クレートドキュメンテーション](https://docs.rs/reqwest/latest/reqwest/)
- [Rustプログラミング言語公式ウェブサイト](https://www.rust-lang.org/ja)
- [RustでWebスクレイピングする方法（Qiita）](https://qiita.com/attsun1031/items/9d95a