---
title:    "Ruby: コンピュータプログラミング：コマンドライン引数の読み込み"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## なぜ？

プログラミングを学ぶ中で、様々な方法でコードを実行することに慣れてくると思います。しかし、コマンドライン引数を読み取るという方法は、より効率的にプログラムを実行するための重要なスキルです。そこで今回は、Rubyでコマンドライン引数を読み取る方法について紹介します。

## 方法

コマンドライン引数を読み取るには、`ARGV`という変数を使用します。例えば、以下のようなコードを実行すると、引数として渡した値が`ARGV`の配列に格納されます。

```Ruby
ruby my_program.rb arg1 arg2 arg3
```

`my_program.rb`の中で、`ARGV[0]`は`arg1`を、`ARGV[1]`は`arg2`を、`ARGV[2]`は`arg3`を表します。複数の引数を読み取る場合は、それぞれのインデックスを指定することで値にアクセスできます。

また、`ARGV`は文字列型の配列であるため、整数や浮動小数点数に変換したい場合は、`to_i`や`to_f`メソッドを使用して変換する必要があります。

## ディープダイブ

コマンドライン引数をより詳細に扱いたい場合、`OptionParser`というクラスを使用することができます。このクラスを使用すると、より高度なコマンドライン引数の読み取りやバリデーションが可能になります。

例えば、以下のように使用することができます。

```Ruby
require 'optparse'

options = {}
OptionParser.new do |opts|
  opts.banner = "使い方: my_program.rb [オプション]"

  opts.on("-f", "--file FILE", "ファイル名を指定") do |file|
    options[:file] = file
  end
  opts.on("-t", "--type TYPE", "種類を指定") do |type|
    options[:type] = type
  end

  opts.on("-h", "--help", "使い方を表示") do
    puts opts
    exit
  end
end.parse!

puts "ファイル名：#{options[:file]}" if options[:file]
puts "種類：#{options[:type]}" if options[:type]
```

このように`OptionParser`を使用することで、コマンドライン引数をより柔軟に扱うことができます。

## 参考リンク

- [Ruby - ARGF](https://ruby-doc.org/core-2.7.1/ARGF.html)
- [Ruby - OptionParser](https://ruby-doc.org/stdlib-2.7.1/libdoc/optparse/rdoc/OptionParser.html)
- [RubyでARGVを読み取る方法](https://qiita.com/ohr486/items/7dbd4348b82de72166e4)