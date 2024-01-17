---
title:                "「二つの日付を比較する」"
html_title:           "Ruby: 「二つの日付を比較する」"
simple_title:         "「二つの日付を比較する」"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 何が & 何故?
日付を比較するとは、プログラマーがある日付が他の日付より前になっているか、同じか、または後ろになっているかを決定することです。プログラマーは、例えば、日付が古い順に並んだリストを作るなど、アプリケーションで日付を処理する必要があるため、日付を比較します。

## 方法:
```Ruby
date1 = Date.new(2021, 7, 16) # 2021年7月16日
date2 = Date.new(2021, 7, 12) # 2021年7月12日

date1 > date2 # true
date2 < date1 # true
date1 == date2 # false
```

## 深堀り:
日付を比較する方法は、時代が変わるにつれて進化してきました。古い方法は、日付を数値として表し、比較することでした。しかし、現在のプログラミング言語では、日付をオブジェクトとして扱うことで、より簡単に比較することができます。また、比較する日付のタイムゾーンにも注意する必要があります。

日付を比較する代替手段としては、数値以外にも文字列や日付の配列を使用する方法があります。しかし、オブジェクトを使用することで、より正確な比較が可能になります。

日付を比較するには、日付が表す情報を取得し、その情報を比較することが重要です。例えば、年、月、日、時、分などの情報を取得し、同じ順序で比較する必要があります。

## 参考:
- [Rubyの公式ドキュメント](https://docs.ruby-lang.org/ja/latest/class/Date.html)
- [【初心者向け】日付の比較方法を学ぼう](https://techacademy.jp/magazine/13509)