---
title:                "Rust: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

#なぜ？

Rustで現在の日付を取得するのは、プログラマーとして基本的なスキルです。現在の日付は、多くのアプリケーションにとって重要な情報であり、時刻管理やデータ処理においても不可欠です。さらに、Rustの標準ライブラリには日付を扱うための便利な機能がたくさんあります。

## 方法

Rustで現在の日付を取得するには、標準ライブラリの`chrono`モジュールを使用します。まず、`chrono`モジュールをプロジェクトに追加する必要があります。

```Rust
use chrono::{Local, DateTime, TimeZone};
```

次に、`Local`構造体を使用して現在のタイムゾーンの情報を取得し、`DateTime`構造体を使用して現在の日付と時刻を取得します。最後に、`format()`メソッドを使用して任意の書式で日付と時刻をフォーマットします。

```Rust
let local = Local::now();
let date_time: DateTime<Local> = local.to_datetime();
println!("{}", date_time.format("%Y/%m/%d %H:%M:%S"));
```

上記のコードを実行すると、現在の日付と時刻が指定した書式で表示されます。

```
2021/01/01 12:00:00
```

さらに、`chrono`モジュールには、日付や時刻の計算や変換を行うための様々なメソッドが用意されています。詳細な使い方は公式ドキュメントを参照してください。

## 深堀り

`chrono`モジュールは、日付や時刻を正確に扱うために、ISO 8601規格に基づいた内部表現を使用します。これにより、様々なタイムゾーンや夏時間の変更にも対応することができます。また、`DateTime`構造体には、日付と時刻の両方を扱うことができるため、柔軟な日付処理が可能です。

また、`chrono`モジュールには、`Duration`構造体や`Period`構造体を使用することで、日付や時刻の計算をより簡単に行うことができます。

See Also

- [Rust公式ドキュメント](https://doc.rust-lang.org/std/time/)
- [Chronoクレート](https://docs.rs/chrono/0.4.19/chrono/)

現在の日付を取得する方法は、プログラミング言語や環境によって異なる場合がありますが、Rustであれば`chrono`モジュールを使うことで簡単に実装することができます。この記事を参考に、ぜひ日付処理をマスターしてください！