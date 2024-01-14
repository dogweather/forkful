---
title:    "Ruby: 「未来または過去の日付を計算する方法」"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

### なぜ
プログラマーにとって、日付の計算は非常に重要です。未来や過去の日付を正確に計算することで、アプリケーションやシステムの動作を予測することができます。そのため、Rubyで日付を計算する方法を知ることは非常に役に立ちます。

### ハウトゥ
日付を計算するためには、Rubyの日付クラスである`Date`を使用します。まず、計算したい日付を`Date`オブジェクトに変換します。その後、`Date`オブジェクトの`+`や`-`演算子を使用して、未来や過去の日付を計算することができます。

```Ruby
require 'date'

# 今日の日付を取得
today = Date.today

# 未来の日付を計算（1日後）
future_date = today + 1

puts future_date #=> 2021-05-05

# 過去の日付を計算（1日前）
past_date = today - 1

puts past_date #=> 2021-05-03
```

### ディープダイブ
日付を計算する際には、`Date`クラスに用意されている様々なメソッドを使用することができます。例えば、`next_day`や`prev_day`メソッドを使用することで、次の日や前の日を計算することができます。また、`strftime`メソッドを使用することで、日付のフォーマットをカスタマイズすることもできます。

```Ruby
require 'date'

# 今日の日付を取得
today = Date.today

# 次の日を計算
next_day = today.next_day
puts next_day.strftime("%d/%m/%Y") #=> 04/05/2021

# 前の日を計算
prev_day = today.prev_day
puts prev_day.strftime("%d/%m/%Y") #=> 02/05/2021
```

`Date`クラスの詳細な情報については、公式ドキュメントを参照することができます。

### ぜひ参考にしてみてください
この記事を参考にして、Rubyで日付を計算する方法をマスターしてください。また、`Date`クラス以外にも、`Time`クラスや`DateTime`クラスなど、日付を扱うためのさまざまなクラスが用意されていますので、ぜひ深堀りしてみてください。

### 関連リンク
- [Ruby公式ドキュメント](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/Date.html)
- [Dateクラスの使い方](https://www.techscore.com/blog/2015/03/02/double_date/)
- [Rubyで日付を扱う方法](https://qiita.com/yamacraft/items/691cc64bcaf6b5f3b4b4)