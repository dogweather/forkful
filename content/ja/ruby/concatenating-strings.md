---
title:                "文字列を連結する"
html_title:           "Ruby: 文字列を連結する"
simple_title:         "文字列を連結する"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

何となく話が始まったとしても、何を言ってるのか全然わからないですよね。だからまず簡単に、Stringの連結というのが何かを説明しましょう。

## 何 & なんで？
Stringの連結とは、文字列を結合することを指します。例えば、"Hello"と"World"という文字列を連結すれば、"HelloWorld"という新しい文字列ができます。プログラムでこれをやる理由は、より複雑な処理をする際に文字列をより効率的に扱えるようにするためです。

## 方法:
Rubyで文字列を連結するには、`+`演算子を使います。例えば、以下のコードを実行すると、`HelloWorld`という文字列が出力されます。

```Ruby
puts "Hello" + "World"
```

他にも、`.concat`メソッドや`<<`演算子でも連結することができます。

## 深層:
文字列の連結は、プログラミング言語でよく使われる操作です。以前の言語では、文字列を結合するために特別な関数が必要でしたが、Rubyでは`+`演算子を使うことでより簡単に扱うことができます。また、文字列ではなくても、配列などのデータ型でも同じように連結することができます。

## 関連リンク:
- [Rubyドキュメント: String](https://docs.ruby-lang.org/en/2.6.0/String.html)
- [プログラミング用語集: 文字列連結](https://wa3.i-3-i.info/word15905.html)
- [Rubyで使える演算子一覧](https://docs.ruby-lang.org/ja/2.6.0/syntax/assignment_rdoc.html)