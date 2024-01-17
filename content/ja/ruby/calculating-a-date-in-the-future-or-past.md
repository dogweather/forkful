---
title:                "未来や過去の日付を計算する"
html_title:           "Ruby: 未来や過去の日付を計算する"
simple_title:         "未来や過去の日付を計算する"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 何となぜ?
日付計算とは、今日の日付を基準に未来や過去の日付を算出することです。プログラマーは、処理やアルゴリズムを作るために日付計算を使用します。

## 方法:
```Ruby
require 'date'

# 未来の日付を計算する
today = Date.today
future_date = today + 10 # 10日後の日付を計算
puts future_date #=> 2020-07-09

# 過去の日付を計算する
past_date = today - 7 # 7日前の日付を計算
puts past_date #=> 2020-06-22
```

## 深堀り:
日付計算は、プログラムでよく使用される機能の一つです。実際には、数学的な処理や複雑なアルゴリズムが関わっていることがあります。代替手段として、プログラマーはよくライブラリやフレームワークを使用して日付計算を行います。日付計算の実装方法はプログラミング言語やライブラリによって異なりますが、一般的には日付を数値で表し、その数値を操作することで日付計算を行います。

## 関連リンク:
- [Rubyの日付計算ドキュメント] (https://docs.ruby-lang.org/ja/2.6.0/class/Date.html)
- [日付計算のアルゴリズム] (https://www.algorist.com/category/datecalc.html)
- [Rubyの日付計算ライブラリ] (https://datebook.dev/)