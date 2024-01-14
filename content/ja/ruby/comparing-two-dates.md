---
title:                "Ruby: 「二つの日付を比較する」"
simple_title:         "「二つの日付を比較する」"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## なぜ

日付を比較することの重要性は、プログラミングにおいてよくある作業の一つです。日付を比較することで、例えばイベントの優先順位を決めたり、期限を設定したりすることができます。

## 使い方

日付を比較する方法はいくつかありますが、ここではRubyを使った方法を紹介します。まず最初に、以下のように記述して日付を変数に代入します。

```Ruby
first_date = Date.new(2020, 1, 1)
second_date = Date.new(2021, 1, 1)
```

上記の例では、それぞれ「2020年1月1日」と「2021年1月1日」を表す日付を変数に代入しています。

次に、以下のように比較演算子「<」や「>」を使って日付を比較します。

```Ruby
if first_date < second_date
  puts "first_date is earlier than second_date"
elsif first_date > second_date
  puts "first_date is later than second_date"
else
  puts "Both dates are the same"
end
```

実行すると、結果は「first_date is earlier than second_date」となります。

また、日付を文字列として扱う場合は、`Date.parse()`メソッドを使用することで簡単に日付を比較することができます。

```Ruby
date_string1 = "2020-01-01"
date_string2 = "2020-01-02"

if Date.parse(date_string1) < Date.parse(date_string2)
  puts "date_string1 is earlier than date_string2"
elsif Date.parse(date_string1) > Date.parse(date_string2)
  puts "date_string1 is later than date_string2"
else
  puts "Both dates are the same"
end
```

実行すると、結果は「date_string1 is earlier than date_string2」となります。

## 深堀り

日付の比較にはさまざまな場面で必要になることがあります。例えば、ある日付がある期間内に含まれるかどうかを判定するときや、ある日付から何日後の日付を求めるときなどです。

また、日付を比較する際は、日付が同じだけでなく時間や時間帯も考慮する必要があります。そのため、日時やタイムゾーンを扱えるようにするためのライブラリを使用することも重要です。

## 参考文献

- RubyのDateクラスのドキュメント: https://docs.ruby-lang.org/en/master/Date.html
- Rubyで日付を比較する方法: https://www.rubyguides.com/2018/08/compare-date-time/
- Rubyで日付を扱う際の注意点: https://qiita.com/Lukeman/items/b8aad2991b28c729ae8c

# 参考文献を見る

- Comparable moduleのドキュメント: https://docs.ruby-lang.org/en/master/Comparable.html
- 時差を考慮した日時を扱うためのTimezoneライブラリ: https://github.com/ankane/timezone
- 時差を計算するためのgemライブラリ一覧: https://rubygems.org/search?utf8=%E2%9C%93&query=timezone