---
title:                "現在の日付を取得する"
html_title:           "Rust: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ

コンピュータープログラミングで、現在の日付を取得する理由を説明します。現在の日付は、ソフトウェア開発やデータ分析など、さまざまなプロジェクトで必要とされる重要な情報です。

## 使い方

Rustで現在の日付を取得する方法を学ぶための簡単なコード例を紹介します。

```Rust
// ライブラリをインポート
use std::time::{SystemTime, UNIX_EPOCH};

// 現在の日付を取得
let now = SystemTime::now();

// エポックからの経過秒数を取得
let seconds = now.duration_since(UNIX_EPOCH).unwrap().as_secs();

// 変数"now"の値を表示
println!("現在の日付: {}", seconds);
```

以上のコードを実行すると、現在の日付をエポックからの経過秒数として表示できます。これにより、日付をさまざまな形式で取得することができます。

## ディープダイブ

Rustで現在の日付を取得する方法には、さまざまなオプションがあります。例えば、タイムゾーンを指定して日付を取得したり、特定の形式で表示したりすることができます。また、UNIXエポック以外の日付を取得する方法もあります。

Rustの公式ドキュメントやコミュニティのサポートを活用することで、さらに詳細な情報を学ぶことができます。

## 参考リンク

* [Rust公式ドキュメント](https://doc.rust-lang.org/std/time/struct.SystemTime.html)
* [Rustコミュニティフォーラム](https://users.rust-lang.org/)