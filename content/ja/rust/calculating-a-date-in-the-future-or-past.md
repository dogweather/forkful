---
title:                "「将来または過去の日付を計算する」"
html_title:           "Rust: 「将来または過去の日付を計算する」"
simple_title:         "「将来または過去の日付を計算する」"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なぜ
人々が過去や未来の日付を計算することに参加するのかを最大2つの文で説明します。

多くの人々は、特定の日付が何曜日なのか、また特定の日数を加算および減算する必要がある場合に、日付計算が必要になります。また、スケジュール管理や予定の立て方を効率的に行うためにも、日付計算は重要なスキルとなります。

## 使い方
日付計算は、様々なプログラムやアプリケーションで使われるようになっています。Rustを使用して日付を計算する方法を見てみましょう。

まずは、使用する標準ライブラリをインポートします。
```Rust
use std::time::{Duration, SystemTime};
```

次に、`SystemTime`を使用して現在の日付時刻を取得します。
```Rust
let current_date = SystemTime::now();
```

未来の日付を計算するには、`Duration`を使用します。例えば、5日後の日付を計算する場合は以下のコードを使用します。
```Rust
let future_date = current_date + Duration::from_secs(86400 * 5);
```
`Duration::from_secs()`で秒単位で日数を指定し、その日数分を加算することで未来の日付が得られます。

過去の日付を計算する場合も同じように行います。例えば、2週間前の日付を計算するには以下のコードを使用します。
```Rust
let past_date = current_date - Duration::from_secs(86400 * 14);
```

## 深堀り
日付計算では、日付を表すデータ型として`SystemTime`を使用することができます。また、日数や時間を表すデータ型として`Duration`を使用することもできます。

`SystemTime`は、実際の日付時刻を取得することができるデータ型です。`Duration`は、指定した単位（秒、ミリ秒、マイクロ秒など）で時間や日数を表すことができます。

日付計算では、特定の日数を加算したり減算したりする必要があるため、`SystemTime`と`Duration`を組み合わせて使用することが一般的です。

## 関連リンクをご覧ください
- [Rust公式ドキュメント - 日付と時刻の操作](https://doc.rust-lang.org/std/time/)
- [Progate - Rust入門コース](https://prog-8.com/languages/rust)