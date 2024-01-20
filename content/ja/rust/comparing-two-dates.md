---
title:                "2つの日付を比較する"
html_title:           "Elixir: 2つの日付を比較する"
simple_title:         "2つの日付を比較する"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 何となぜ？

日付を比較するとは、2つの日付の前後関係、同じか否かを判断することです。これは、有効期限が過ぎているか判定したり、イベントを順番に並べるためなどにプログラマーが使用します。

## どうやるのか

```Rust
use std::cmp::Ordering;
use std::time::SystemTime;

fn main() {
    let now = SystemTime::now();
    let past = SystemTime::UNIX_EPOCH;

    match now.duration_since(past) {
        Ok(_) => println!("現在時刻はUNIX起点時刻より後です。"),
        Err(_) => println!("現在時刻はUNIX起点時刻より前です。"),
    }

    match now.cmp(&past) {
        Ordering::Less => println!("現在時刻はUNIX起点時刻より前です。"),
        Ordering::Equal => println!("現在時刻はUNIX起点時刻と同じです。"),
        Ordering::Greater => println!("現在時刻はUNIX起点時刻より後です。"),
    }
}
```

サンプル出力:

```sh
現在時刻はUNIX起点時刻より後です。
現在時刻はUNIX起点時刻より後です。
```

## 詳細情報

日付の比較機能は、Rust言語が生まれてから以降ずっと存在しています。当初は`SystemTime::duration_since()`関数を使用しましたが、比較可能な型で実装することで、より直感的で簡単に比較できるようになりました。

代替案として`chrono`クレートを使用することも可能です。`chrono`は、より包括的な日付・時刻操作を可能にします。

Rustで日付を比較する際には`std::cmp::Ordering`を使います。これは、日付の大きさを比較するためのenumで、`Less`、`Equal`、`Greater`の3種類の値を返すことができます。

## 参考情報

1. [Rustの日付と時刻](https://stevedonovan.github.io/rustifications/2018/09/08/common-rust-lifetime-misconceptions.html)
2. [`chrono` crate](https://docs.rs/chrono/0.4.19/chrono/)
3. [Rustでの日付の操作](https://doc.rust-jp.rs/book-ja/ch12-01-accepting-command-line-arguments.html)