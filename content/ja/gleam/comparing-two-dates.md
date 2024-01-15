---
title:                "「二つの日付を比較する」"
html_title:           "Gleam: 「二つの日付を比較する」"
simple_title:         "「二つの日付を比較する」"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## ワイ: なぜ二つの日付を比べることが重要か？

日付を比べることは、日々のタスクを管理するために不可欠です。特定の日付の前後を知ることで、計画を立てたり期限を設定したりすることができます。Gleamの比較機能を使うことで、簡単かつ正確に日付の比較ができるようになります。

## ハウツー: 日付の比較の方法

まずは、Gleamの標準ライブラリからDateモジュールをインポートします。

```Gleam
import gleam/date
```

次に、比較したい二つの日付をそれぞれ定義します。

```Gleam
let first_date = date.make(2021, 3, 20)
let second_date = date.make(2021, 7, 5)
```

日付の比較は、Dateモジュールのcmp関数を使用します。この関数は、二つの日付の前後関係を表すOrder型を返します。例えば、first_dateがsecond_dateよりも前の場合は、Order.Lessが返されます。

```Gleam
let result = date.cmp(first_date, second_date)
```

最後に、比較結果を使用して条件分岐を行うことで、二つの日付を比べることができます。

```Gleam
if result == Order.Less {
  // first_dateがsecond_dateよりも前の場合の処理
} else if result == Order.Equal {
  // 二つの日付が同一の場合の処理
} else {
  // first_dateがsecond_dateよりも後の場合の処理
}
```

## ディープダイブ: 日付の比較について

GleamのDateモジュールには、日付をより詳細に扱うための様々な関数があります。例えば、日付の加算や減算を行うためのadd関数やsub関数、曜日を取得するためのweekday関数などがあります。また、Dateモジュールのドキュメントを参照することで、さらに詳しい情報を得ることができます。

## その他：関連リンク

- Gleamのドキュメント: https://gleam.run/documentation/
- Dateモジュールのドキュメント: https://gleam.run/modules/gleam-date/latest/Date.html