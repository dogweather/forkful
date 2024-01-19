---
title:                "コマンドライン引数の読み取り"
html_title:           "Bash: コマンドライン引数の読み取り"
simple_title:         "コマンドライン引数の読み取り"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 何となぜ？

コマンドライン引数を読み取るとは、ユーザーが直接パラメータを入力できる方法のことを指します。プログラマはこれを利用して、一般ユーザーがプログラムの動作を独自に制御できるようにするために使用します。

## 使い方:

以下のスクリプトを見てみましょう。

```ruby
# hello_world.rb
name = ARGV.first
puts "Hello, #{name}!"
```
コマンドラインから実行すると次のようになります。

```sh
$ ruby hello_world.rb Japan
Hello, Japan!
```

ARGVはargsの配列です。そのため、indexを使用して直接アクセスすることも可能です。

## ディープダイブ:

### 史上

UNIXで標準化されたシェルプログラミングと同じコンセプトにRubyも沿っています。この方法はプログラムとユーザーの間の対話を可能にし、同時にコードの再利用性も高めます。

### 代替案

OptionParserライブラリや、ARGVをより高度に操作するためのgem(最も人気のあるものはThorとTrollop)のような強力なツールもあります。

### 実装詳細

ARGV配列は直接操作することが可能で、ARGV[0]のように索引を用いて要素にアクセスできます。ARGV.eachを使用すればARGV配列の全要素を繰り返し処理することも可能です。

## 関連情報:

- Rubyの公式ドキュメンテーションの[Command Line Arguments](https://www.ruby-lang.org/en/documentation/quickstart/4/) 
- [OptionParser](https://ruby-doc.org/stdlib-2.1.0/libdoc/optparse/rdoc/OptionParser.html)
- Thor gemの[Github repository](https://github.com/erikhuda/thor)
- Trollop gemの[Github repository](https://github.com/RubyMoney/money)