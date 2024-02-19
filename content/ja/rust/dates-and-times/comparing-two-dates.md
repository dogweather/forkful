---
aliases:
- /ja/rust/comparing-two-dates/
date: 2024-01-20 17:33:55.262410-07:00
description: "\u65E5\u4ED8\u3092\u6BD4\u8F03\u3059\u308B\u3063\u3066\u3044\u3046\u306E\
  \u306F\u3001\u5358\u7D14\u306B\u7570\u306A\u308B\u4E8C\u3064\u306E\u65E5\u4ED8\u306E\
  \u524D\u5F8C\u95A2\u4FC2\u3092\u78BA\u304B\u3081\u308B\u3053\u3068\u3060\u3002\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u6709\u52B9\u671F\u9650\u306E\u30C1\u30A7\
  \u30C3\u30AF\u3084\u30A4\u30D9\u30F3\u30C8\u306E\u30B9\u30B1\u30B8\u30E5\u30FC\u30EB\
  \u8ABF\u6574\u306A\u3069\u3001\u8272\u3005\u306A\u5834\u9762\u3067\u65E5\u4ED8\u6BD4\
  \u8F03\u3092\u884C\u3046\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:54.732625
model: gpt-4-1106-preview
summary: "\u65E5\u4ED8\u3092\u6BD4\u8F03\u3059\u308B\u3063\u3066\u3044\u3046\u306E\
  \u306F\u3001\u5358\u7D14\u306B\u7570\u306A\u308B\u4E8C\u3064\u306E\u65E5\u4ED8\u306E\
  \u524D\u5F8C\u95A2\u4FC2\u3092\u78BA\u304B\u3081\u308B\u3053\u3068\u3060\u3002\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u6709\u52B9\u671F\u9650\u306E\u30C1\u30A7\
  \u30C3\u30AF\u3084\u30A4\u30D9\u30F3\u30C8\u306E\u30B9\u30B1\u30B8\u30E5\u30FC\u30EB\
  \u8ABF\u6574\u306A\u3069\u3001\u8272\u3005\u306A\u5834\u9762\u3067\u65E5\u4ED8\u6BD4\
  \u8F03\u3092\u884C\u3046\u3002"
title: "\u65E5\u4ED8\u3092\u6BD4\u8F03\u3059\u308B"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)

日付を比較するっていうのは、単純に異なる二つの日付の前後関係を確かめることだ。プログラマーは、有効期限のチェックやイベントのスケジュール調整など、色々な場面で日付比較を行う。

## How to: (方法)

```Rust
use chrono::{DateTime, Utc};

fn main() {
    // 2つの日付をUTCで生成
    let date1: DateTime<Utc> = "2023-04-01T12:00:00Z".parse().unwrap();
    let date2: DateTime<Utc> = Utc::now();

    // date1がdate2よりも前かどうかをチェック
    if date1 < date2 {
        println!("date1 is earlier than date2");
    } else {
        println!("date1 is not earlier than date2");
    }
}
```

実行結果には以下のような出力が含まれます（日付の値による）：

```
date1 is earlier than date2
```

## Deep Dive (深掘り)

比較する前に、`chrono`クレートが必要だ。`chrono`はRustの日付と時間を扱うためのライブラリで、高い柔軟性と正確さを兼ね備えている。さらに、タイムゾーンに基づいた比較も可能になる。

過去、Rust標準の`std::time`モジュールが日付操作を提供していたが、機能が限られており、多くの場合、`chrono`クレートが代わりに用いられる。

`DateTime<Utc>`を使用し、二つの日付をUTCで取り扱っている。時差に影響されない厳密な比較が可能。`parse()`メソッドを使って文字列から日付をパースし、`Utc::now()`で現在の日付と時間を取得する。そして、比較演算子`<`を使ってどちらが前かをチェックする。

## See Also (参照)

- [chrono crate documentation](https://docs.rs/chrono/)
- [Rust `std::time` module documentation](https://doc.rust-lang.org/std/time/)
