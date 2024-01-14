---
title:                "Rust: 「現在の日付を取得する」"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ
Rustで現在の日付を取得するのは重要ですか？

Rustは現代的なプログラミング言語であり、その才能に満ちています。Rustはメモリ安全性と高速性を備えているため、多くの開発者にとって魅力的なオプションとなっています。現在の日付を取得することは、プログラムの実行中に時間に関する情報を必要とする様々なアプリケーションにとって重要な要素です。そのため、Rustで現在の日付を取得する方法を知ることは、より効率的で正確なプログラムを作成する上で重要なスキルの1つです。

## 使い方
```rust
use std::time::SystemTime;
use chrono::{DateTime, Utc};

fn main() {
    let current_time = SystemTime::now().duration_since(SystemTime::UNIX_EPOCH).unwrap();
    let current_datetime = DateTime::<Utc>::from(current_time);
    println!("{}", current_datetime);
}
```

上記のコードを実行すると、現在の日付がUTC形式で出力されます。まず、`SystemTime::now()`を使用して現在の時間を取得し、`UNIX_EPOCH`との差を計算して時刻を取得します。それから、`chrono`ライブラリを使用して日付をUTC形式に変換し、`println!`マクロを使ってコンソールに出力します。このコードは、簡単にカスタマイズすることができ、ローカルタイムゾーンにも対応できます。

## 深堀り
`SystemTime`と`chrono`ライブラリは、現在の日付を取得するための強力なツールです。`SystemTime`は、UNIXエポック（1970年1月1日00:00:00 UTC）からの経過時間を表すstructであり、プログラムの開始時に取得した`SystemTime`と比較することで、任意の時点の経過時間を計算することができます。また、`chrono`ライブラリは、タイムゾーンの変換や日付のフォーマットなど、さまざまな機能を提供しています。これらのツールを組み合わせることで、より正確な日付を取得することができます。

## See Also
- [Rustドキュメント](https://doc.rust-lang.org/std/time/struct.SystemTime.html)
- [Chronoライブラリドキュメント](https://docs.rs/chrono/0.4.19/chrono/)
- [Rust入門ガイド](https://www.rust-lang.org/learn/get-started)