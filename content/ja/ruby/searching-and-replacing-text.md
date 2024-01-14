---
title:                "Ruby: テキストの検索と置換"
simple_title:         "テキストの検索と置換"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## なぜ

テキストを検索して置換することは、コーディングにおいて重要なタスクです。特定の文字列を一括で置き換えることで、プログラムの動作や出力を変更することができます。この記事では、Rubyを使ってテキストを検索・置換する方法を紹介します。

## 使い方

まずは検索する文字列を指定します。例えば、次のようなコードがあったとします。

```
text = "Hello, world!"
```

もし、このコードの中の"Hello"を"Hi"に置き換えたい場合、次のようにコードを書きます。

```
text.gsub!("Hello", "Hi")
```

すると、`text`の文字列が"Hi, world!"に置き換わります。ただし、この置換は最初にマッチした文字列のみに適用されます。

複数の文字列を一括で置き換えたい場合には、正規表現を使うことができます。例えば、次のようなコードがあったとします。

```
text = "Hello, world! Ruby is awesome!"
```

ここで「Hello」と「Ruby」をそれぞれ「Hi」と「Python」に置き換えたい場合、正規表現を使って次のようにコードを書きます。

```
text.gsub!(/Hello|Ruby/, "Hi" => "Python")
```

すると、`text`の文字列が"Hi, world! Python is awesome!"に置き換わります。

また、置き換えた後の文字列をすべて大文字や小文字に変換することもできます。例えば、先ほどのコードを次のように書き換えると、文字列がすべて大文字に変換されます。

```
text.gsub!(/Hello|Ruby/, "Hi" => "Python".upcase)
```

## 深堀り

テキストを検索・置換する際、正規表現を使うことでより柔軟な操作が可能になります。正規表現を使うことで、マッチするパターンや置き換える内容を自在に指定することができます。しかし、正規表現は少し複雑なため、使いこなすには練習が必要です。正規表現のパターンを作成する際には、[Rubular](https://rubular.com/)や[Rubyの公式ドキュメント](https://docs.ruby-lang.org/ja/latest/doc/spec=2fregexp.html)などのサイトを参考にすると便利です。

## その他の情報

Rubyにおけるテキストの検索と置換については、[Stringクラスのドキュメント](https://docs.ruby-lang.org/ja/latest/class/String.html#I_GSUB)を参照してください。

## 関連リンク

- [Ruby Cheat Sheets](https://www.ruby-lang.org/ja/documentation/cheat-sheet/)
- [正規表現チュートリアル](https://www.regular-expressions.info/tutorial.html)
- [Rubyを使ったテキスト操作のヒント](https://launchschool.com/blog/handling-text-in-ruby)
- [Rubyの正規表現入門](http://www.geocities.jp/kosako3/oniguruma/guideline-ja.html)