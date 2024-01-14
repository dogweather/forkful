---
title:    "Gleam: 将来または過去の日付を計算する"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なぜ

日付を未来や過去に計算する必要があるかを簡潔に説明します。

多くのプログラミングプロジェクトでは、日付を計算する必要が出てきます。例えば、注文した商品の配送予定日や、イベントの開催日を計算する必要があります。Gleamを使用すると、日付の計算をより簡単に行うことができます。

## 方法

日付の計算をGleamで行う方法を以下のコードブロックを使用して説明します。

```Gleam
import gleam/time

// 現在の日付を取得
let current_date = time.now()

// 1日後の日付を計算
let tomorrow = time.add_day(current_date, 1)

// 1か月前の日付を計算
let last_month = time.add_month(current_date, -1)

// 計算した日付のフォーマットを指定
let formatted_date = time.format_date(last_month, "%Y-%m-%d")

// 結果を出力
gleam/io.println("Last month was " <> formatted_date)
```

上記の例では、```time```モジュールを使用して現在の日付を取得し、その日付を基にして1日後や1か月前の日付を計算し、指定したフォーマットで出力しています。Gleamの日付計算機能を活用することで、簡単に複雑な日付の計算を行うことができます。

## ディープダイブ

日付の計算を行う際には、タイムゾーンや夏時間などの概念にも注意が必要です。Gleamでは、```locale```モジュールを使用することで、タイムゾーンや地域ごとの異なる日付の表現を扱うことができます。また、```calendar```モジュールを使用することで、暦に関連する計算を行うことも可能です。

日付の計算を行う際には、現在の日付やタイムゾーン設定、フォーマットなどについてよく確認し、必要に応じてGleamのモジュールを使用しましょう。

## おわりに

この記事を読んでGleamで日付の計算が行えるようになったかと思います。以下のリンクを参考に、さらにGleamの機能を活用してみてください。

## 関連リンク

- [Gleam公式ドキュメント](https://gleam.run/documentation/)
- [日付処理モジュールの参考ページ](https://gleam.run/documentation/standard-library/time/)
- [タイムゾーン設定の方法](https://gleam.run/documentation/standard-library/time/#locale-and-timezone)