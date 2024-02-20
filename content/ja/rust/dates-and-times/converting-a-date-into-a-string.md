---
date: 2024-01-20 17:37:23.963070-07:00
description: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B\u3068\
  \u306F\u3001`DateTime`\u69CB\u9020\u4F53\u3092\u8AAD\u307F\u53D6\u308A\u3084\u3059\
  \u3044\u30C6\u30AD\u30B9\u30C8\u5F62\u5F0F\u306B\u5909\u3048\u308B\u3053\u3068\u3067\
  \u3059\u3002\u30C7\u30FC\u30BF\u306E\u30ED\u30AE\u30F3\u30B0\u3001\u30E6\u30FC\u30B6\
  \u30FC\u30A4\u30F3\u30BF\u30FC\u30D5\u30A7\u30FC\u30B9\u3001\u307E\u305F\u306F\u30C7\
  \u30FC\u30BF\u306E\u30B7\u30EA\u30A2\u30E9\u30A4\u30BC\u30FC\u30B7\u30E7\u30F3\u306B\
  \u3088\u304F\u4F7F\u308F\u308C\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:01.012435
model: gpt-4-1106-preview
summary: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B\u3068\
  \u306F\u3001`DateTime`\u69CB\u9020\u4F53\u3092\u8AAD\u307F\u53D6\u308A\u3084\u3059\
  \u3044\u30C6\u30AD\u30B9\u30C8\u5F62\u5F0F\u306B\u5909\u3048\u308B\u3053\u3068\u3067\
  \u3059\u3002\u30C7\u30FC\u30BF\u306E\u30ED\u30AE\u30F3\u30B0\u3001\u30E6\u30FC\u30B6\
  \u30FC\u30A4\u30F3\u30BF\u30FC\u30D5\u30A7\u30FC\u30B9\u3001\u307E\u305F\u306F\u30C7\
  \u30FC\u30BF\u306E\u30B7\u30EA\u30A2\u30E9\u30A4\u30BC\u30FC\u30B7\u30E7\u30F3\u306B\
  \u3088\u304F\u4F7F\u308F\u308C\u307E\u3059\u3002"
title: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
日付を文字列に変換するとは、`DateTime`構造体を読み取りやすいテキスト形式に変えることです。データのロギング、ユーザーインターフェース、またはデータのシリアライゼーションによく使われます。

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
