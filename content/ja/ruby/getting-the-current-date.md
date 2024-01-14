---
title:                "Ruby: 現在の日付の取得"
programming_language: "Ruby"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ
「現在の日付を取得する」という作業の重要性は、プログラミング言語を学ぶ上で必須のスキルです。日付を取得することで、タイムスタンプを作成したり、アプリケーションにおいて特定の日付を追跡することができます。

## 方法
日付を取得するには、Rubyの標準ライブラリである`Date`クラスを使います。まずは、`require`メソッドを使って`date`ライブラリを読み込みます。

```Ruby
require 'date'

```

次に、`Date.today`メソッドを使って、今日の日付を取得することができます。

```Ruby
today = Date.today
```

もしくは、任意の日付を取得することもできます。例えば、2022年8月20日を取得するには以下のように記述します。

```Ruby
specific_date = Date.new(2022, 8, 20)
```

また、時刻情報を含めたい場合は、`DateTime`クラスを使います。

```Ruby
now = DateTime.now
```

これらのコードを実行すると、日付や時刻に関する情報を含むオブジェクトが返されます。

## 深堀り
さらに詳細な日付情報を取得したい場合は、`Date`や`DateTime`クラスのメソッドを使うことで可能です。例えば、特定の日付が何曜日かを知りたい場合は、`wday`メソッドを使います。

```Ruby
date = Date.new(2022, 8, 20)
puts date.wday # => 6 （土曜日）
```

また、年や月、日など単独の情報を取得することもできます。

```Ruby
specific_date = Date.new(2022, 8, 20)
puts specific_date.year # => 2022
puts specific_date.month # => 8
puts specific_date.day # => 20
```

さまざまなメソッドを組み合わせることで、必要な日付情報を取得することができます。

## さらに詳しく学ぶ
以上が日付を取得するための基本的な方法ですが、実はRubyにはもっと高度な日付操作の方法が備わっています。例えば、時間差やタイムゾーンを扱うことも可能です。

より詳しい情報を学びたい方は、以下のリンクをチェックしてみてください。

[Dateクラスのドキュメント](https://ruby-doc.org/stdlib/libdoc/date/rdoc/Date.html)
[DateTimeクラスのドキュメント](https://ruby-doc.org/stdlib/libdoc/date/rdoc/DateTime.html)

## 関連情報を参照する
[Ruby基礎文法まとめ](https://qiita.com/yatnnn/items/f6defe1ff96d9a2faf56)