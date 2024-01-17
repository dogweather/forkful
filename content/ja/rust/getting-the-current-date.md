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

## 何が必要であるか？
現在の日付を取得することは、プログラマーにとって重要なことです。これにより、特定の日付に関連するアクションを実行したり、時間に基づいて動作を制御したりすることができます。

## 方法：
現在の日付を取得するには、```Rust ... ```コードブロック内のサンプルコードを使用します。

```Rust
use std::time::SystemTime;

fn main() {
  let current_time = SystemTime::now();
  println!("Current Date: {:?}", current_time);
}
```

出力：

```
Current Date: Tue, 09 Mar 2021 10:35:12 GMT
```

## 深く掘り下げる
日付を取得する方法にはさまざまな方法があります。Rustでは、標準ライブラリの中にある `time` モジュールから `SystemTime` 構造体を使用することで現在の日付を取得できます。また、 `chrono` ライブラリを使用する方法もあります。

## 参考
- [Rust Documentation on time](https://doc.rust-lang.org/std/time/)
- [Chrono crate for handling dates and times in Rust](https://github.com/chronotope/chrono)