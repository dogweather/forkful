---
title:                "テキストファイルを書く"
html_title:           "Rust: テキストファイルを書く"
simple_title:         "テキストファイルを書く"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

## 何 & なぜ？
文章を書くことは、プログラマーにとって重要な作業です。テキストファイルとは、プレーンテキストで書かれたファイルのことです。プログラマーは、プログラムやデータを保存するためにテキストファイルを使用します。

## 方法：
Rustでは、標準ライブラリの```std::fs```モジュールを使用して、テキストファイルを作成することができます。以下のコードを参考にしてください。

```Rust
use std::fs::File;
use std::io::prelude::*;

fn main() {
    // ファイルを作成し、書き込み可能なファイルハンドラーを取得
    let mut file = File::create("sample.txt").unwrap();

    // ファイルにデータを書き込む
    let _ = file.write(b"Hello from Rust!");

    // ファイルを閉じる
    let _ = file.flush();
}
```

上記のコードでは、ファイルを作成し、指定した内容を書き込んでいます。また、ファイルを閉じることで、変更を確定させることができます。

## 深く掘り下げる：
テキストファイルは、プログラムやデータを保存するための一般的な方法として、長い歴史を持っています。また、テキストファイルの代わりにバイナリファイルを使用することも可能ですが、プログラマーの間ではテキストファイルが好まれる傾向にあります。Rustでは、ファイルの読み書きを簡単に行うことができるため、多くのプログラマーから支持されています。

## 関連リンク：
- [Rust標準ライブラリのドキュメント](https://doc.rust-lang.org/std/fs/index.html)
- [Rustのファイル操作についての記事「Rustでファイルを扱う方法」](https://aruno14.medium.com/rust-%E3%81%A7%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB%E3%82%92%E6%89%B1%E3%81%86%E6%96%B9%E6%B3%95-3a47fb023708)
- [テキストファイルとバイナリファイルの違いについての記事「テキストファイルとバイナリファイル、それぞれの違い」](https://proengineer.internous.co.jp/content/columnfeature/9877)