---
title:                "Ruby: パターンに一致する文字を削除する"
simple_title:         "パターンに一致する文字を削除する"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ

あなたが文字列を操作しているとき、特定のパターンにマッチする文字を削除する必要があるかもしれません。そのような場合、Rubyプログラミング言語を使うと簡単に文字を削除することができます。今回の記事では、どのようにして文字を削除するかを詳しく説明します。

## どのようにして削除するか

Rubyでは、 `gsub`というメソッドを使って文字を削除することができます。以下の例を見てください。

```Ruby
text = "これはサンプルテキストです。"
new_text = text.gsub(/サンプル/, "")
puts new_text
```

上記のコードでは、 `gsub`メソッドを使用して、 `サンプル`という文字列を空文字列で置き換えています。すると、出力は `これはテキストです。` となります。

ただし、`gsub` メソッドの第1引数には、削除したいパターンを正規表現で指定する必要があります。さらに、削除したい文字列が複数ある場合は、正規表現の中で `+` を使用して一括で指定することができます。

```Ruby
text = "カレーコロッケとオムライスが大好きです。"
new_text = text.gsub(/カレー|メンチカツ/, "")
puts new_text
```

上記のコードでは、 `カレー` と `メンチカツ` を削除しています。出力は `コロッケとオムライスが大好きです。` となります。

## ディープダイブ

では、 `gsub` メソッドについてもう少し詳しく見てみましょう。 `gsub` メソッドは、文字列の代わりにブロックを渡すことができます。その場合、ブロックの戻り値が文字列の置き換えに使用されます。

```Ruby
text = "今日は寒いです。"
new_text = text.gsub(/(.+)/) { |match| match.reverse }
puts new_text
```

ブロック内では、マッチした文字列が `|match|` 変数に渡されます。上記のコードでは、 `今日は寒いです。` が取得され、 `.reverse` メソッドを使って文字列を逆順にすると `。すていは日今` となり、それが出力になります。

## See Also

- [Ruby 公式ドキュメント - String#gsub](https://docs.ruby-lang.org/ja/latest/method/String/i/gsub.html)
- [Ruby API リファレンス - String#gsub](https://docs.ruby-lang.org/ja/2.7.0/method/String/i/gsub.html)
- [Rubyガイド - gsub メソッド](https://ruby-guide.readthedocs.io/ja/latest/ruby/methods/gsub.html)