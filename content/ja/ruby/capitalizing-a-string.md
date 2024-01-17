---
title:                "文字列のキャピタライズ"
html_title:           "Ruby: 文字列のキャピタライズ"
simple_title:         "文字列のキャピタライズ"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 何でしょう & なぜ: StringをCapital化することとプログラマーがそれをする理由を説明します。
文字列のCapital化とは、最初の文字を大文字に変換することです。これにより、より読みやすいコードを作成することができます。プログラマーは、コードを読みやすくするためにしばしばStringをCapital化します。

## 方法: ```Ruby```コードブロック内のコーディング例とサンプル出力を参照してください。
```ruby
name = "john"
puts name.capitalize # "John"
age = 25
puts age.to_s.capitalize # "25"
```

## 詳しく: (1)歴史的背景、(2)代替方法、および(3)StringをCapital化する実装の詳細について説明します。
文字列のCapital化は、英語の文章で最初の文字を大文字にすることから始まりました。これは、名詞や文の先頭を大文字にする英語の文法規則に基づいています。代替方法としては、Stringを全て大文字もしくは小文字に変換するメソッドもあります。StringをCapital化する実装には、最初の文字を抽出し、それを大文字に変換するアルゴリズムが使われています。

## 関連情報を参照: 関連する情報源へのリンク。
- [Rubyドキュメント - class String](https://docs.ruby-lang.org/ja/latest/class/String.html)
- [Stringを大文字に変換する方法](https://ruby-doc.org/core-2.7.0/String.html#method-i-capitalize)
- [Stringを小文字に変換する方法](https://ruby-doc.org/core-2.7.0/String.html#method-i-downcase)