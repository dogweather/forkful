---
title:                "パターンにマッチする文字を削除する"
html_title:           "Ruby: パターンにマッチする文字を削除する"
simple_title:         "パターンにマッチする文字を削除する"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 何? なぜ?

文字パターンに一致する文字を削除することは、プログラマーが行う一般的な作業です。これは、特定の条件を満たさない文字を削除することで、コードの効率性や読みやすさを向上させるためです。

## 方法:

### 文字列から特定の文字を削除する方法:
```Ruby 
str = "Hello World"
newStr = str.delete("l") 
puts newStr
```
出力:
```
Heo Word
```

### 正規表現を使って文字列から特定の文字を削除する方法:
```Ruby 
str = "12345678"
newStr = str.gsub(/[13]/, "")
puts newStr
```
出力:
```
245678
```

### 配列から特定の文字を削除する方法:
```Ruby 
arr = ["apple", "banana", "orange"]
newArr = arr - ["banana"]
puts newArr
```
出力:
```
["apple", "orange"]
```

## 深堀り:

### 歴史的な背景:
文字パターンに一致する文字を削除するというアイデアは、古くからあるものです。例えば、古代ローマの数学者であるオッキャムのウィリアムが提唱した「オッキャムの剃刀の原理」に由来し、不要なものを削除することで合理的な解決を図るという考え方があることを示しています。

### 代替方法:
文字パターンに一致する文字を削除する他の方法としては、特定の文字を検索してから削除する方法や、文字列の一部を取り除く方法があります。それぞれの方法にはメリット・デメリットがあり、状況に応じて使い分けることが大切です。

### 実装の詳細:
Rubyでは、StringクラスやArrayクラスのメソッドを使って文字パターンに一致する文字を削除することができます。具体的には、.deleteメソッドや.gsubメソッド、-演算子を使うことができます。これらのメソッドや演算子は、文字列や配列を変更せずに新しいオブジェクトを返します。

## 関連リンク:

- [Rubyドキュメンテーション](https://docs.ruby-lang.org/ja/2.6.0/class/String.html#I_DELETE)
- [Rubyガイドブック](https://ruby-book.jnito.com/basics/syntax/string)
- [正規表現チュートリアル](https://ruby-rails.hatenadiary.com/entry/20141231/1420020245)