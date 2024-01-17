---
title:                "文字列の長さを求める"
html_title:           "Ruby: 文字列の長さを求める"
simple_title:         "文字列の長さを求める"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 何をしているのか？なぜするのか？

文字列の長さを見つけるとは、文字列に含まれている文字の数を数えることです。プログラマーがこれをする理由は、コードで使用している文字列の数を把握し、処理を正しく行うためです。

## 方法：

```ruby
# 方法1：`.length`メソッドを使用する
"こんにちは".length #=> 5

# 方法2：`.size`メソッドを使用する
"こんにちは".size #=> 5

# 方法3：`String#count`メソッドを使用する
"こんにちは".count("あ-ん") #=> 5
```

## 深く掘り下げる

文字列の長さを見つける方法はいくつかありますが、どのメソッドが最も効率的かは、使用するコードや環境によって異なります。また、文字列の長さを見つけるために正規表現を使用することもできます。しかし、文字列の長さを見つける方法の中で最も一般的で効率的な方法は、`.length`や`.size`メソッドを使うことです。

## 関連リンク

- [Rubyドキュメンテーション - `.length`メソッド](https://docs.ruby-lang.org/en/3.0.0/String.html#method-i-length)
- [Rubyドキュメンテーション - `.size`メソッド](https://docs.ruby-lang.org/en/3.0.0/String.html#method-i-size)
- [Rubyドキュメンテーション - `String#count`メソッド](https://docs.ruby-lang.org/en/3.0.0/String.html#method-i-count)