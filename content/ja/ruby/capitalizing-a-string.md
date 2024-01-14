---
title:    "Ruby: 文字列を大文字にする"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## なぜ

あなたがプログラミングをしているなら、文字列を大文字に変換することは非常によく使う機能です。例えば、ユーザーの名前やメッセージを大文字に変換して表示することがよくあります。Rubyでは、文字列を簡単に大文字に変換することができます。では、どのようにすれば良いのでしょうか？

## 使い方

文字列を大文字に変換するには、```upcase```メソッドを使用します。たとえば、以下のコードを試してみましょう。

```Ruby
word = "hello"
puts word.upcase
```

このコードを実行すると、出力は "HELLO" になります。また、数字や記号も同様に大文字に変換されます。

```Ruby
sentence = "hello, I am a Ruby programmer!"
puts sentence.upcase
```

このコードを実行すると、出力は "HELLO, I AM A RUBY PROGRAMMER!" になります。簡単ですね！

## 深堀り

文字列を大文字に変換することは、Rubyでは非常に簡単に行うことができますが、これを実現する仕組みはどのようなものでしょうか？実は、Rubyでは文字列を```String```クラスのオブジェクトとして扱います。このクラスには多くのメソッドがあり、その中に文字列を大文字に変換する```upcase```メソッドも含まれています。このメソッドを使用することで、オブジェクト自体を変更せずに大文字に変換された新しい文字列を返すことができます。

## もっと詳しく知りたい方へ

もし、Rubyの文字列についてもっと学びたいという方は、以下のサイトを参考にしてください。

- [Rubyの公式ドキュメント](https://docs.ruby-lang.org/en/2.6.0/String.html)
- [Ruby Primerの文字列のセクション](https://ruby-doc.org/core-2.6.3/String.html)
- [Rubyモノリスの文字列の部分](https://ruby.jtalks.jp/String) 

## 関連記事
- [Rubyの文字列連結方法](https://webruby.net/string/concat/)
- [Rubyの数値を文字列に変換する方法](https://webruby.net/convert_to_str/)
- [Rubyの配列を文字列に変換する方法](https://blog.takuyan.com/entry/2019/02/04/184540)