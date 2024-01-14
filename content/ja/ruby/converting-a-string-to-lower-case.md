---
title:    "Ruby: 文字列を小文字に変換する"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ

文字列を小文字に変換することには、多くの理由があります。例えば、文字列を比較する際に大文字と小文字の違いを無視したい場合、またはデータの整形を行う際に一貫性を持たせたい場合などです。

## 方法

文字列を小文字に変換するには、Stringクラスのdowncaseメソッドを使用します。下記のように記述します。

```ruby
"HELLO WORLD".downcase
```

これにより、"hello world"という出力が得られます。

## ディープダイブ

Rubyでは、文字列を変換する際にロケール（言語や地域）の差異を考慮することができます。例えば、日本語の場合は半角英数字を全角に変換する必要があります。このような場合、downcaseメソッドの引数にロケールを指定することができます。

```ruby
"ｈｅｌｌｏ ｗｏｒｌｄ".downcase(:ja)
```

上記の例では、"hello world"という出力が得られます。ただし、ロケールによって変換される文字列の種類や仕様は異なる場合がありますので、注意が必要です。

## 関連リンク

[Rubyドキュメント](https://ruby-doc.org/core-2.7.2/String.html#method-i-downcase)<br>
[Rubyのロケールについて](https://ruby-doc.org/core-2.7.2/I18n.html)<br>
[Ruby on Railsガイド](https://railsguides.jp/i18n.html)