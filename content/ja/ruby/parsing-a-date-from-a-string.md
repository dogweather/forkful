---
title:                "文字列から日付を解析する"
html_title:           "Ruby: 文字列から日付を解析する"
simple_title:         "文字列から日付を解析する"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 何となに？
日付を文字列からパースするとは何か、そしてプログラマーがそれをする理由は何かを説明します。

日付を文字列からパースするとは、文字列から日付として意味を持つ情報を抽出することです。例えば、「2020年10月10日」という文字列から「2020年10月10日」という日付を取り出すことができます。

プログラマーが日付を文字列からパースする理由は、日付をデータとして処理する必要があるからです。例えば、ユーザーが入力した日付をデータベースに保存する場合、文字列ではなく日付として正しく処理する必要があります。

## やり方：
```Ruby
# 文字列からDateオブジェクトを作成する方法
Date.parse("2020-10-10")
#=> #<Date: 2020-10-10 ((2459142j,0s,0n),+0s,2299161j)>

# strftimeメソッドを使用して日付をフォーマットする方法
date = Date.new(2020, 10, 10)
date.strftime("%Y年%m月%d日")
#=> "2020年10月10日"

# 文字列からDateTimeオブジェクトを作成する方法
DateTime.parse("2020-10-10 12:00")
#=> #<DateTime: 2020-10-10T12:00:00+00:00 ((2459142j,43200s,0n),+0s,2299161j)>
```

## 深く掘り下げる：
- 日付を文字列からパースする必要があった背景には、コンピューターの日付表現の歴史があります。かつては、日付を数字や特殊な書式で表していましたが、現代のプログラミング言語では、日付をオブジェクトとして扱うことが一般的になりました。
- 文字列から日付をパースする方法は、他にも多数あります。例えば、正規表現を使って特定のパターンの日付を抽出する方法や、外部ライブラリを使用する方法があります。
- 日付を文字列からパースする際、プログラミング言語がどのように文字列を解釈するかに注意する必要があります。例えば、英語の場合は「月/日/年」、日本語の場合は「年/月/日」のように、日付の表記には言語によって違いがあります。

## 関連情報：
- [Dateクラスのドキュメント](https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/Date.html)
- [DateTimeクラスのドキュメント](https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/DateTime.html)
- [strftimeメソッドのドキュメント](https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/DateTime.html#method-i-strftime)
- [外部ライブラリ「Chronic」の紹介記事](https://qiita.com/yasunori_fujii/items/fb12c139c11b43eba5c9)