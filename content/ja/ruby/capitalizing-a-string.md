---
title:                "文字列の大文字化"
html_title:           "Ruby: 文字列の大文字化"
simple_title:         "文字列の大文字化"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ

文字列を大文字に変換する理由はいくつかあります。例えば、入力された文字列を正規化するためや、プログラムで処理する際に一貫性を保つためです。また、単に見た目を美しくするためにも利用されます。

## 方法

### 文字列を大文字に変換する方法

入力された文字列を大文字に変換するには、RubyのStringクラスにあるupcaseメソッドを使用します。また、downcaseメソッドを使用すると文字列を小文字に変換することもできます。以下のコードを参考にしてください。

```Ruby
input = "Hello World"
puts input.upcase # 出力結果: HELLO WORLD
puts input.downcase # 出力結果: hello world
```

### 文字列の最初の文字を大文字に変換する方法

文字列の最初の文字を大文字に変換するには、capitalizeメソッドを使用します。以下のコードを参考にしてください。

```Ruby
input = "hello world"
puts input.capitalize # 出力結果: Hello world
```

### 文字列の各単語の最初の文字を大文字に変換する方法

文字列の各単語の最初の文字を大文字に変換するには、titleizeメソッドを使用します。以下のコードを参考にしてください。

```Ruby
input = "hello world"
puts input.titleize # 出力結果: Hello World
```

## ディープダイブ

Rubyでは、Stringクラスに多数のメソッドが用意されており、文字列の大文字・小文字変換だけでなく、部分文字列の取得や置換など、さまざまな処理が可能です。また、文字コードによる大文字・小文字の違いも考慮されています。詳細な情報は公式ドキュメントを参照してください。

## 参考リンク

- [Rubyドキュメント: Stringクラス](https://docs.ruby-lang.org/ja/latest/class/String.html)
- [RubyGuides: Rubyの文字列操作ガイド](https://www.rubyguides.com/2019/06/ruby-string-methods/)
- [Progate: Rubyの文字列操作について学ぼう](https://prog-8.com/docs/ruby-string)