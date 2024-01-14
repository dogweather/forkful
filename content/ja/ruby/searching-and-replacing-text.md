---
title:                "Ruby: テキストの検索と置換"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# なぜ検索と置換をするのか

検索と置換は、プログラミングの世界で非常に重要なタスクです。テキスト内の特定のキーワードを見つけて置換することで、数百、数千、または数万ものファイルを効率的に更新することができます。これにより、時間の節約やヒューマンエラーの可能性を減らすことができます。Rubyプログラミング言語を使えば、簡単で強力な検索と置換を実装することができます。

## 検索と置換の方法

Rubyでは、Stringクラスのメソッドである`gsub`を使って検索と置換を行います。`gsub`メソッドは、指定した文字列を別の文字列に置き換えます。下の例を見てみましょう。

```Ruby
"Hello World".gsub("Hello", "こんにちは")
```

このコードを実行すると、"Hello World"が"こんにちは World"に置換されます。もし、テキスト内に複数の"Hello"がある場合は、すべての"Hello"が置換されます。

また、正規表現を使って検索と置換を行うこともできます。正規表現を使うと、より柔軟な検索が可能になります。下の例では、`/o/`という正規表現を使って、単語内の"o"を"0"に置換しています。

```Ruby
"Hello World".gsub(/o/, "0")
```

このコードを実行すると、"H0ll0 W0rld"という文字列が返されます。

## 検索と置換の詳細

もっと深く検索と置換を理解するために、`gsub`メソッドの内部を見てみましょう。Rubyのドキュメントによると、`gsub`メソッドは次のように定義されています。

```Ruby
gsub(pattern, replacement) → new_str
```

第1引数には検索する文字列または正規表現を、第2引数には置換する文字列を指定します。検索した文字列が見つかった場合は、置換された新しい文字列が返されます。

また、`gsub!`というメソッドもあります。このメソッドは、呼び出し元の文字列を直接置換することで、メモリの使用量を減らすことができます。

## さらに学ぶために

検索と置換はプログラミングの中でも非常に便利なタスクです。もし興味があれば、ぜひRubyのドキュメントやチュートリアルを読んで、もっと多くの情報を学んでください。

## 他のリソース

- [Ruby ドキュメント](https://ruby-doc.org/core-2.7.1/String.html#method-i-gsub)
- [Ruby ガイド](https://rubyguides.com/ruby-string-methods/)
- [正規表現チュートリアル](https://www.regular-expressions.info/tutorial.html)