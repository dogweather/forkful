---
title:                "Gleam: 将来または過去の日付の計算"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# なぜ
日付を計算する必要があるのかを説明するため、まずは「なぜ」について考えてみましょう。時には、特定の日付を過去や未来に進めたり、戻したりする必要があります。例えば、予定されたイベントの日時をチェックするために、ある日付から数週間前や数ヶ月後の日付を計算する必要があるかもしれません。そんな時に便利なのが、Gleamの日付計算機能です。

# 使い方
実際に日付を計算する方法を見てみましょう。以下は、ある日付から3日後の日付を計算するコードの例です。

```Gleam
let start_date = Date.from_iso8601("2021-01-01")
let end_date = start_date |> Date.add_days(3)
```

上記のコードを実行すると、`end_date`には2021年1月4日が入ります。このように、Gleamでは簡単に日付を計算することができます。

# 詳細を掘り下げる
日付を計算する際には、様々なオプションがあります。例えば、特定の日付から数日前や数ヶ月後の日付を計算するだけでなく、特定の曜日や月初めなど特定の条件を満たす日付を計算することもできます。また、日付のフォーマットを変更することも可能です。

さらに、Gleamはエラー処理にも対応しており、問題が起きた際に適切なエラーメッセージを出力することができます。これにより、安心して日付計算を行うことができます。

# 併せて読みたい
もし日付計算に興味があるのであれば、以下のリンクも参考にしてみてください。

- [Gleam公式ドキュメント](https://gleam.run/documentation/)
- [Gleam GitHubリポジトリ](https://github.com/gleam-lang/gleam)

それでは、日付計算を活用して、より便利なGleamプログラミングを楽しんでください！