---
date: 2024-01-20 17:37:23.963070-07:00
description: "How to: (\u65B9\u6CD5) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.840079-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B"
weight: 28
---

## How to: (方法)
```Rust
use chrono::{DateTime, Utc, Local};

fn main() {
    // UTCで現在の日時を取得して文字列に変換
    let now_utc: DateTime<Utc> = Utc::now();
    println!("{}", now_utc.format("%Y-%m-%d %H:%M:%S").to_string());

    // ローカルタイムゾーンで現在の日時を取得して文字列に変換
    let now_local: DateTime<Local> = Local::now();
    println!("{}", now_local.format("%Y-%m-%d %H:%M:%S").to_string());
}
```
サンプル出力:
```
2023-03-14 12:34:56
2023-03-14 21:34:56 // ローカルタイムゾーンに依存
```

## Deep Dive (深掘り)
Rustで日付を文字列に変換するとき、`chrono`クレートが一般的です。これはRustが標準で提供している日時ライブラリよりも柔軟性とパワーがあります。`chrono`は.NETの`DateTime`やJavaの`SimpleDateFormat`に触発されました。

選択肢として、`time`クレートもありますが、機能は`chrono`の方が豊富です。したがって、複雑な日付処理が必要な場合は`chrono`が良いでしょう。

変換の実装時、`format`メソッドを使っています。このメソッドでは、フィールドに対応する形式指定子を用いて、出力のフォーマットを細かく指定できます。例えば`%Y`は4桁の年を、`%m`は月を2桁で表します。これはC言語の`strftime`関数からの影響を受けています。

## See Also (参照)
- [Chronoクレートのドキュメント](https://docs.rs/chrono/)
- [strftimeの形式指定子](http://man7.org/linux/man-pages/man3/strftime.3.html)
