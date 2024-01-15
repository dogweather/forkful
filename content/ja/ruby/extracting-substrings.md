---
title:                "「部分文字列を抽出する」"
html_title:           "Ruby: 「部分文字列を抽出する」"
simple_title:         "「部分文字列を抽出する」"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ

サブストリングを抽出することの利点は何でしょうか？それは、与えられた文字列から特定の部分を簡単に取得することができるからです。文字列操作をする際に、サブストリングを抽出することが非常に便利です。

## 抽出の方法

```Ruby
str = "今日はいい天気です。"
puts str[0..2] # 「今日」が出力される
puts str[6..8] # 「いい天気」が出力される
```

サブストリングを抽出するには、文字列のインデックスを使用します。上記の例では、str[開始インデックス..終了インデックス]という形式で抽出しています。ここで注意する点は、終了インデックスの数値は抽出される文字数に含まれることです。また、インデックスは0から始まることにも注意してください。

## ディープダイブ

サブストリングの抽出には、実際にはさまざまな方法があります。最も基本的な方法は、文字列のインデックスを使用することですが、`slice()`や`substring()`などのメソッドを使用することもできます。また、正規表現を使用して抽出することも可能です。

## ご参考までに

- [Rubyドキュメント - Stringクラス](https://docs.ruby-lang.org/en/latest/String.html)
- [Stringクラスメソッド一覧 - Qiita](https://qiita.com/chimu/items/8137743467ac97278f53) 

## 参考文献
- https://docs.ruby-lang.org/en/latest/String.html
- https://qiita.com/chimu/items/8137743467ac97278f53