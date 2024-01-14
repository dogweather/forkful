---
title:                "Ruby: 文字列の連結"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

# なぜ文字列の連結をするのか

Rubyプログラミングを学ぶ上で、文字列の連結は非常に重要なスキルです。様々な理由がありますが、最も一般的な理由はデータの整形や出力のためです。例えば、ユーザーがフォームに入力した情報を受け取り、それを一つの文章として表示したいときなどに使われます。

# 方法

文字列の連結には、Rubyの組み込みメソッドである`+`を使用します。例えば、次のコードを見てみましょう。

```Ruby
first_name = "太郎"
last_name = "山田"

full_name = first_name + last_name

p full_name
```

上のコードでは、`first_name`と`last_name`を連結して`full_name`という新しい変数を作成しています。そして、`p`メソッドを使って`full_name`を出力しています。結果は`太郎山田`と表示されます。

また、文字列の連結には`<<`や`concat`というメソッドもあります。これらは`+`と同じく、文字列同士を連結するためのものです。

# 深堀り

文字列の連結を行う際には、注意点があります。例えば、次のコードを見てみましょう。

```Ruby
a = "Hello"
b = "World"

a << b
```

このコードでは、`a`という変数に対して`<<`を使用して`b`を連結しています。しかし、この場合は`a`の値自体が変更されてしまいます。つまり、`a`は`HelloWorld`という文字列を持つことになります。

これを避けるためには、`+`を使用して新しい変数を作成するか、`a`を複製する必要があります。例えば、次のように書くことで変数自身は変更せずに文字列を連結することができます。

```Ruby
a = "Hello"
b = "World"

new_string = a + b

p new_string # => "HelloWorld"
p a # => "Hello"
```

# 参考リンク

- [Rubyドキュメンテーション: String](https://ruby-doc.org/core-2.7.0/String.html)
- [Rubyの文字列を連結する方法](https://qiita.com/okamos/items/5437b69fee76006bce0a)
- [Rubyの文字列の基本操作](https://www.sejuku.net/blog/19041)

# 参考文献

- [Rubyドキュメンテーション: String](https://ruby-doc.org/core-2.7.0/String.html)
- [Rubyの文字列を連結する方法](https://qiita.com/okamos/items/5437b69fee76006bce0a)
- [Rubyの文字列の基本操作](https://www.sejuku.net/blog/19041)