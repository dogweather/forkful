---
title:                "テキストの検索と置換"
html_title:           "Java: テキストの検索と置換"
simple_title:         "テキストの検索と置換"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 何となぜ？

テキストの検索と置換は特定の文字列を探し、必要に応じて別の文字列で置換する手続きです。プログラマーはこの手続きを用いて、ソースコードの更新、データの修正、パターンの追跡などを効果的に行うために使用します。

## 使い方：

```Ruby
# 検索と置換の基本的な使い方
text = "Hello, World!"
replaced_text = text.gsub("World", "Japan")
puts replaced_text
```

出力：

```Ruby
Hello, Japan!
```

ここで、"World"という文字列を"Japan"と置き換えました。gsubメソッドはすべての一致を探して置き換えますが、subメソッドを使用すると最初の一致だけを置き換えます。

```Ruby
# 最初の一致のみ置換
text = "Hello, World! World!"
replaced_text = text.sub("World", "Japan")
puts replaced_text
```

出力：

```Ruby
Hello, Japan! World!
```

## ディープダイブ：

**歴史的背景**：Rubyはテキストとその操作を容易に扱うことを目的とした言語であり、検索と置換はその一部です。これはPerlという言語に触発された部分で、Perlは文字列操作を強力にサポートしていました。

**代替方法**：Rubyではgsubとsubの他にも、文字列の検索と置換に使用できるslice!や[]=などのメソッドがあります。

```Ruby
word = 'Hello, World!'
word['World'] = 'Japan'
puts word
```

出力：

```Ruby
Hello, Japan!
```

**実装の詳細**：gsubとsubメソッドは、正規表現による一致検索をサポートしています。これにより、独自の一致パターンを定義することが可能となります。

```Ruby
text = "Hello, World!"
replaced_text = text.gsub(/[A-Z]/, "*")
puts replaced_text
```

出力：

```Ruby
*****, ******!
```

## 参考資料：

1. [Ruby Docs 文字列](https://docs.ruby-lang.org/ja/latest/class/String.html): Rubyの公式ドキュメンテーションにはString classの詳細があります。
2. [正規表現チュートリアル](https://www.regular-expressions.info/tutorial.html): このチュートリアルでは、正規表現の基本から応用まで学習できます。