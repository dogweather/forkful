---
title:                "Ruby: 文字列の長さの求め方"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ

文字列の長さを求めることに取り組む理由は、プログラミングにおいて文字列が重要な役割を果たすためです。文字列の長さを求めることで、プログラムの処理に役立つ情報を得ることができます。

## やり方

文字列の長さを求めるには、Rubyの `length` メソッドを使います。

```Ruby
my_string = "こんにちは"
puts my_string.length
# => 5
```

上記のように、文字列の変数に `length` メソッドを呼び出すことで、その文字列の長さを取得することができます。例では、日本語の文字列であっても正しく長さを取得できることがわかります。

## 深ぼり

実は、 `length` メソッドが呼び出されるとき、内部的には文字列の各文字をループ処理していることがわかります。つまり、文字列の長さを求めるときには、プログラマーが意識するよりも多くの処理が行われているのです。しかし、これは文字列の長さを求めるためには必要な処理であり、Rubyの `length` メソッドが効率的に動作していることがわかります。

## 参考リンク

- [Ruby ドキュメント - String#length](https://docs.ruby-lang.org/ja/latest/class/String.html#I_LENGTH)
- [文字列の長さを取得する方法](https://www.sejuku.net/blog/25440)
- [Ruby で文字数を数える際の注意点](https://qiita.com/hisamura333/items/62217fb8bf9e0e9cdf43)