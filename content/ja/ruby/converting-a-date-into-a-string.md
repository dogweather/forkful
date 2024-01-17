---
title:                "日付を文字列に変換する"
html_title:           "Ruby: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 何か？

Rubyでは、日付を文字列に変換することができます。日付を文字列に変換するとは、日付の形式を変更することを指します。プログラマーは、このような変換を行うことで、テキスト形式の日付をデータベースに保存したり、ユーザーに表示したりすることができます。

## 方法：

Rubyの```strftime```メソッドを使用することで、日付を文字列に変換することができます。以下のコードを参考にしてください。

```ruby
date = Date.new(2021, 9, 1)
puts date.strftime("%Y年%m月%d日")
#=> 2021年09月01日
```

```strftime```メソッドでは、日付を任意の形式でフォーマットすることができます。詳細なフォーマット指定子については、公式ドキュメントを参照してください。

## 詳細：

日付を文字列に変換する方法は、プログラミング言語によって異なります。Rubyの場合は、組み込みの```strftime```メソッドを使用することができますが、他の言語ではライブラリを使用する必要があるかもしれません。

また、日付を文字列に変換する際には、ロケールによって表示が異なることに注意が必要です。例えば、日本では月と日の表記が西洋と異なるため、適切に変換する必要があります。

## 関連リンク：

- [Rubyの公式ドキュメント（strftimeメソッド）](https://docs.ruby-lang.org/en/3.0.0/DateTime.html#method-i-strftime)
- [strftimeについてのブログ記事](https://tando-re.com/ruby/date-string/)
- [ロケールについての説明（日本語）](https://www.sharcnet.jp/help/index.php/UNIX%E3%83%A6%E3%83%86%E3%82%A3%E3%83%AA%E3%83%86%E3%82%A3%E3%81%AE%E8%AA%9E%E5%BD%99%E6%84%8F%E5%91%B3)