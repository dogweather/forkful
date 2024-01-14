---
title:                "Ruby: 「現在の日付の取得」"
simple_title:         "「現在の日付の取得」"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ

プログラミングをする方々にとって、現在の日付を取得することは非常に役立ちます。特にプログラム内で日付を使用する場合には、正確な日付が必要になります。そこで今回は、Rubyで現在の日付を取得する方法を紹介します。

## 方法

RubyのDateTimeモジュールを使用することで、現在の日付や時刻を簡単に取得することができます。以下のようにコードを書くことで、現在の日付や時刻を取得することができます。

```Ruby
require 'date'

# 現在の日付を取得する
date = Date.today
puts "今日の日付は#{date}です。"

# 現在の時刻を取得する
time = Time.now
puts "現在の時刻は#{time}です。"
```

上記のコードを実行すると、以下のような出力結果が得られます。

```
今日の日付は2020-08-06です。
現在の時刻は2020-08-06 12:00:00 +0900です。
```

また、日付や時刻を特定のフォーマットで取得することもできます。例えば、年月日を「年-月-日」のフォーマットで取得する場合は、以下のようにコードを書きます。

```Ruby
require 'date'

# 現在の日付を「年-月-日」のフォーマットで取得する
date = Date.today.strftime("%Y-%m-%d")
puts "今日の日付は#{date}です。"
```

この場合、今日の日付は「2020-08-06」というフォーマットで取得されます。

## 深堀り

RubyのDateTimeモジュールには、現在の日付や時刻を取得する以外にもさまざまな便利な機能があります。例えば、「日付や時刻の計算」「日付や時刻の比較」「特定の曜日の日付を取得する」など、さまざまな機能がありますので、ぜひ試してみてください。

## 参考リンク

- [Date and Timeクラス - Rubyリファレンスマニュアル](https://docs.ruby-lang.org/ja/latest/class/DateTime.html)
- [Rubyで現在時刻を取得する方法 - Qiita](https://qiita.com/terrym/items/f0444a65075c27f33023)
- [RubyのDateTimeで日付と日時を扱う - hiroLabo](https://www.hirolabo.com/archives/tech/3512)

# 他にも見る

- [Rubyの基本的な日付と時間の処理方法 - Prog-8](https://prog-8.com/docs/ruby-date-time-basics)