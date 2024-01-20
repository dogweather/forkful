---
title:                "文字列を大文字にする"
html_title:           "Ruby: 文字列を大文字にする"
simple_title:         "文字列を大文字にする"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列のキャピタライズは、文字列の最初の文字を大文字にすることを指します。プログラマーがこれを行う理由は、適切な読みやすさと一貫性を提供するためです。

## 方法：

Ruby言語では、文字列のキャピタライズは非常に簡単になります。 `capitaliza`メソッドを使用して実行できます。

```Ruby
str = "hello world"
cap_str = str.capitalize
puts cap_str
```
これを実行すると、以下の出力が得られます。

```Ruby
"Hello world"
```

## 深掘り：

文字列のキャピタライズは、プログラミングの初期から存在します。これはある種の標準化を可能にし、読解性を高めるのに役立ちます。

代替策としては、 `upcase` メソッドを使用して文字列全体を大文字にするか、または `downcase` メソッドを用いて文字列全体を小文字にすることもできます。ただし、キャピタライズは特定のケース、特にタイトルや文章の始めなどで最も一般的です。

具体的な実装としては、Rubyは実際に `capitalize`メソッドを呼び出すことにより、文字列の最初の文字を大文字にし、残りの文字を小文字に変換します。

## 参考に：

1. [Ruby API 文書](https://docs.ruby-lang.org/ja/latest/method/String/i/capitalize.html)
2. [文字列をキャピタライズする方法](https://www.rubyguides.com/2018/01/ruby-string-methods/#capitalize)
3. [Ruby String capitalize() メソッド](https://www.geekhideout.com/rubyhacker/ref/rubyref-String.html)