---
title:                "文字列の補間"
html_title:           "Ruby: 文字列の補間"
simple_title:         "文字列の補間"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

文字列の補間とは、データを差し込んでより動的なメッセージを作成することです。プログラマーはこれを行うことで、同じメッセージを簡単に異なる入力値に対応させることができます。

## How to:

```Ruby
# 変数を使用した文字列の補間
name = "John"
age = 25
puts "私の名前は#{name}です。年齢は#{age}歳です。"

# 配列を使用した文字列の補間
fruits = ["apple", "banana", "orange"]
puts "私のお気に入りのフルーツは#{fruits[0]}です。"

# ハッシュを使用した文字列の補間
person = { name: "Emily", age: 30 }
puts "#{person[:name]}の年齢は#{person[:age]}歳です。"
```

出力結果:

私の名前はJohnです。年齢は25歳です。
私のお気に入りのフルーツはappleです。
Emilyの年齢は30歳です。

## Deep Dive:

文字列の補間は、Rubyにおいて比較的新しい機能です。この機能は1992年に生まれたPerl言語から影響を受けており、動的な文字列の作成に便利です。代わりに、Rubyでは文字列演算子を使用することで文字列を結合することができますが、この場合複数の文字列を結合するために`+`を使用する必要があります。

文字列の補間は、変数や配列、ハッシュを含む任意の式を使うことができます。また、複数の式を含む場合は`${...}`を使うことで式を明示的に指定することもできます。

## See Also:

- [Ruby ドキュメンテーション](https://docs.ruby-lang.org/en/master/doc/syntax/literals_rdoc.html#label-String+Interpolation)
- [Ruby on Rails チュートリアル](https://www.railstutorial.org/book/toy_app#code-interpolation_with_rb) 
- [RubyGuides: String Interpolation](https://www.rubyguides.com/2016/06/ruby-string-interpolation/)