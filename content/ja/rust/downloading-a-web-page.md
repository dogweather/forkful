---
title:                "Rust: ウェブページをダウンロード"
simple_title:         "ウェブページをダウンロード"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Why
ウェブページをダウンロードする理由は多岐に渡りますが、その一つにはウェブスクレイピングやデータ収集があります。Rustの強力な並列処理能力と高速性を活かして、有用なデータを簡単かつ効率的に収集することができます。

## How To

ウェブページをダウンロードするためには、Rustの標準ライブラリであるreqwestクレートが必要です。まずはこのクレートをプロジェクトに追加しましょう。
```Rust
[dependencies]
reqwest = "0.11.4"
```

次に、reqwestクレートをインポートします。
```Rust
use reqwest::blocking::get;
```
ウェブページをダウンロードするには、`get()`メソッドを使用します。このメソッドにはダウンロードするページのURLを引数として渡します。例えば、google.comをダウンロードするには次のようになります。
```Rust
let response = get("https://www.google.com").unwrap();
```
レスポンスは、`Response`型のインスタンスとして返されます。このインスタンスからウェブページのHTMLコンテンツを取得するには、`text()`メソッドを使用します。
```Rust
let body = response.text().unwrap();
```
これで、ウェブページのHTMLコンテンツが`body`変数に格納されます。

## Deep Dive
ウェブページをダウンロードする際には、レスポンスのステータスコードやヘッダー情報を確認することも重要です。レスポンスのステータスコードは、`status()`メソッドで取得できます。また、`headers()`メソッドを使用することで、ヘッダー情報を取得することができます。

また、reqwestクレートには非同期制御が可能な非同期メソッドもあります。複数のウェブページを同時にダウンロードする場合は、非同期メソッドを使用することで処理速度を向上させることができます。

## See Also
- [reqwestクレートのドキュメント（英語）](https://docs.rs/reqwest/0.11.4/reqwest/)
- [ウェブスクレイピングにおける注意点（日本語）](https://note.mu/oozou/n/n2b3d9692f9b8)
- [Rustでの非同期プログラミング（日本語）](https://qiita.com/tatsuya6502/items/097c5c078985d26068ce)