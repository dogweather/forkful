---
title:                "将来または過去の日付の計算"
html_title:           "Ruby: 将来または過去の日付の計算"
simple_title:         "将来または過去の日付の計算"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 何となぜ？
未来や過去の日付を計算するとは、特定の日付から数日、数週間、数年先または前の日付を見つけ出すことを指します。プログラマーはこの手法を用いて、アプリケーションでタスクの期限、予約日、イベントのスケジュールなどを管理します。

## 実践方法：
Rubyでは日付計算は簡単に行うことが可能です。以下にその例を示します。
```Ruby
require 'date'
today = Date.today
puts today
# 2021-09-23
future_date = today + 10
puts future_date
# 2021-10-03
past_date = today - 30
puts past_date
# 2021-08-24
```
このコードでは現在の日付に対して日数を加えるか減らすことで未来や過去の日付を計算しています。

## ディープダイブ：
Rubyが歴史的に日付計算を容易にしてきた背景には、元々テキスト処理に強い言語だからです。さらに、その豊富な組み込みライブラリーが日付の計算を支持しています。

他の言語、例えばPythonやJavaでも日付の計算は可能ですが、Rubyの`Date`クラスはとても使いやすく、また直感的なインターフェースを提供している点が優れています。

算術記号（+/-）を使用して日付を計算するため、Rubyの日付計算は他の言語より直感的に理解できます。内部的には、`Date`オブジェクトは日付を「ユリウス日」（紀元前4713年11月24日からの日数）として保持し、これが日付計算を容易にします。

## 参考リンク：
RubyのDateクラスについての詳細は[こちら](https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/Date.html)からご覧ください。

また、タイムゾーンに関する詳細な情報は[これらのリソース](https://guides.rubyonrails.org/v5.2/active_record_querying.html#types-of-arguments)でご覧いただくことができます。

同じことをPythonで行いたい場合は[こちらのリンク](https://docs.python.org/3/library/datetime.html)をご覧ください。