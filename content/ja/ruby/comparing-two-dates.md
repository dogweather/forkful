---
title:                "Ruby: 日付の比較"
programming_language: "Ruby"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## なぜ
「日付の比較」を行う理由は、プログラミングにおいて日付をうまく扱うためには欠かせないからです。

## 方法
日付の比較を行うには、RubyのDateクラスやTimeクラスを使用して行うことができます。以下のコードを参考にしてください。

```Ruby
# Dateクラスを使用した場合
date1 = Date.parse("2020/01/01")
date2 = Date.parse("2020/01/05")

if date1 > date2
  puts "date1はdate2よりも後の日付です。"
elsif date1 < date2
  puts "date1はdate2よりも前の日付です。"
else
  puts "date1とdate2は同じ日付です。"
end

# Timeクラスを使用した場合
time1 = Time.new(2020, 1, 1)
time2 = Time.new(2020, 1, 5)

if time1 > time2
  puts "time1はtime2よりも後の日付です。"
elsif time1 < time2
  puts "time1はtime2よりも前の日付です。"
else
  puts "time1とtime2は同じ日付です。"
end

```

出力結果は以下のようになります。

```
date1はdate2よりも前の日付です。
time1はtime2よりも前の日付です。
```

## 深堀り
日付の比較には、より柔軟に行う方法もあります。例えば、DateクラスやTimeクラスのメソッドを使用することで、特定の日付の部分だけを比較することができます。また、日付のフォーマットに関しても注意する必要があります。詳細な情報は、Rubyの公式ドキュメントやオンラインの資料を参考にしてください。

## See Also
- [Rubyの公式ドキュメント](https://www.ruby-lang.org/ja/documentation/)
- [プロを目指す人のためのRuby入門](https://www.amazon.co.jp/%E3%83%97%E3%83%AD%E3%82%92%E7%9B%AE%E6%8C%87%E3%81%99%E4%BA%BA%E3%81%AE%E3%81%9F%E3%82%81%E3%81%AE%E3%83%93%E3%82%B8%E3%83%8D%E3%82%B9%E7%94%A8-Ruby%E5%85%A5%E9%96%80-%E5%A2%97%E7%94%B0-%E5%A4%AA%E5%85%92/dp/4839930216)