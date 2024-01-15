---
title:                "標準エラーへの書き込み"
html_title:           "Ruby: 標準エラーへの書き込み"
simple_title:         "標準エラーへの書き込み"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## なぜ

標準エラーに書き込むことの目的は、プログラム実行中に発生したエラーを管理することです。エラーメッセージを標準エラーに書き込むことで、開発者はプログラムの実行結果を綿密にチェックし、問題を素早く特定することができます。

## 方法

```ruby
STDERR.puts "This is an error message."
#=> This is an error message.
```

標準エラーへの書き込みには、`STDERR.puts`メソッドを使用します。エラーメッセージを引用符で囲んで引数として渡すことで、メッセージを標準エラーに書き込むことができます。また、` STDERR`はRubyでは標準エラー出力を表す特殊なオブジェクトです。同様に、標準出力を表す` STDOUT`オブジェクトもあります。

## 深堀り

標準エラーでエラーメッセージを出力することは、開発者にとって非常に重要です。エラーメッセージを出力することで、プログラム実行中に発生したエラーの種類や原因を特定し、修正することができます。また、エラーメッセージをログファイルに書き込むことで、将来的なデバッグに役立つ情報を残すことができます。

## 参考リンク

- [Rubyドキュメンテーション - STDERR](https://docs.ruby-lang.org/en/master/IO.html#class-IO-label-Default+Streams)
- [Rubyドキュメンテーション - Kernel.puts](https://docs.ruby-lang.org/en/master/Kernel.html#method-i-puts)
- [The Standard Streams - Ruby for Admins](http://ruby-for-admins.ruby5studios.com/the-standard-streams.html)

##