---
title:                "文字列の補間"
html_title:           "Arduino: 文字列の補間"
simple_title:         "文字列の補間"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ?

文字列補間（Interpolating a string）は、文字列の中に変数や式を埋め込むことであり、その結果、コードの可読性と効率性が高まります。

## やり方：

Rubyでは`#`記号と`{}`を使って文字列補間を行います。これは具体的な例で示すと理解しやすいでしょう。

```Ruby
name = 'Yamada'
greeting = "Hello, #{name}!"

puts greeting 
```

上記のコードでは、nameという変数をgreetingという文字列の中に埋め込んでいます。このコードを実行すると、以下のような出力結果が表示されます。

```
Hello, Yamada!
```

## 深層探索

1. **歴史的背景**: Rubyが誕生した当初から文字列補間の機能は備わっており、他の言語と比較してもRubyの文字列補間は簡単で直感的です。 

2. **代替方法**: 文字列の連結を使って同じ結果を得ることも可能です。ただし、それはコードの見通しを悪くし、複雑な式の場合にはより多くのコードを必要とします。

```Ruby
name = 'Yamada'
greeting = 'Hello, ' + name + '!'

puts greeting
```

3. **実装の詳細**: 文字列補間の際には、`#{}`内の式が先に評価され、その結果が文字列に置き換えられます。式が文字列以外の場合には、自動的に`.to_s`メソッドが適用されます。

```Ruby
x = 10
y = 20
sum = "Sum is #{x + y}"

puts sum
```

このコードを実行すると次のように表示されます：

```
Sum is 30
```

## 関連情報

- [Ruby Documentation: String Interpolation](https://ruby-doc.org/core-2.7.0/doc/syntax/literals_rdoc.html#label-Strings)