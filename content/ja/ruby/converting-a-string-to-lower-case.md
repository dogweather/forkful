---
title:                "Ruby: 「文字列を小文字に変換する」"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ

文字列を小文字に変換する理由は、プログラミングにおいてよく使われる操作の1つです。例えば、ユーザーの入力を検証する場合や、データベースの検索を行う場合に、大文字と小文字を区別せずに比較するために使用されます。

## 方法

文字列を小文字に変換するには、Rubyの `downcase` メソッドを使用します。例えば、以下のようにコードを書くことで、文字列を小文字に変換することができます。

```Ruby
str = "HELLO WORLD"
puts str.downcase
```

このコードの出力結果は、`hello world`となります。

## 詳細説明

Rubyの `downcase` メソッドは、文字列を小文字に変換するだけでなく、国際的な文字列にも対応しています。つまり、半角の大文字や全角の大文字、アクセントのある文字なども正しく小文字に変換することができます。

また、`downcase` メソッドを使用する際に、文字列の先頭を大文字にする `capitalize` メソッドや、単語の先頭の文字を大文字にする `capitalize!` メソッドと組み合わせることで、最初の1文字を大文字にした後に計算された残りの文字を小文字に変換することもできます。

## 他の参考情報

[オンラインRubyチュートリアル](https://www.learnrubyonline.org/ja/conditionals)
[Ruby公式ドキュメント](https://docs.ruby-lang.org/ja/2.7.0/class/String.html#I_DOWNCASE)