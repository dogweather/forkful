---
title:                "文字列を小文字に変換する"
html_title:           "Arduino: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列を小文字に変換するとは、全ての大文字アルファベットを対応する小文字に変換することです。プログラマーがこれを行う理由は主に二つあります。一つは、大文字と小文字を区別しない文字列比較を行うため、もう一つはユーザーが入力したデータを統一するためです。

## どうやって：

以下に、Rubyで文字列を小文字に変換する方法を示します:

```Ruby
str = "Hello, World!"
puts str.downcase
```

出力は次のようになります:

```
hello, world!
```

## ディープダイブ:

過去には、特定の文字列を一般的な形式に変換し、文字列比較を容易にするために小文字変換が使われていました。Rubyのバージョンによっては、小文字変換のアルゴリズムが異なることがあります。

Ruby以外でも、多くのプログラミング言語には同様の文字列を小文字に変換するメソッドが存在します。たとえば、Pythonには `lower()`、JavaScriptには `toLowerCase()` などのメソッドがあります。

また、Rubyでは適用元の文字列自体を変更する`downcase!`というメソッドもあります。しかし、このメソッドは元の文字列を変更するため、注意が必要です。

## こちらもご覧ください:

Rubyの公式ドキュメンテーションの【downcase】メソッドについての詳細な説明は、以下のリンクを参照してください:
http://ruby-doc.org/core/String.html#method-i-downcase

同様に、【downcase!】メソッドについての詳細は以下のリンクをご覧ください:
http://ruby-doc.org/core/String.html#method-i-downcase-21