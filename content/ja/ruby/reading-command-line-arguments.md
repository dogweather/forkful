---
title:                "Ruby: コマンドライン引数の読み取り"
simple_title:         "コマンドライン引数の読み取り"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# なぜ

コマンドライン引数の読み取りを学ぶことは、より多くの経験を得るために役立ちます。また、自分のプログラムが他人にとっても使いやすくなるため、よりプロフェッショナルに見えるようになります。

# 方法

コマンドライン引数を読み取り、使用する方法は簡単です。 ```ARGV```という変数を使用して、引数を読み取ることができます。以下のコードは、オプションの引数を使った例です。

```Ruby
# オプションの引数を読み取るためのコード
option = ARGV[0]

if option == "-h"
  puts "使い方：ruby program.rb <オプション> <ファイル名>"
  puts "-h：使い方を表示する"
  puts "-c：ファイルの内容を表示する"
end

file_name = ARGV[1]

if option == "-c"
  file = File.open(file_name)
  contents = file.read
  puts contents
end
```

このプログラムを実行する際は、以下のようにコマンドラインから引数を渡すことができます。

```
ruby program.rb -h
ruby program.rb -c test.txt
```

# 深堀り

コマンドライン引数を読み取る際、```ARGV```は配列として引数を受け取ります。引数は、最初の要素から順に、0番目、1番目、2番目のように番号が付けられます。また、コマンドライン引数はスペースで区切って入力することができます。

例えば、```ruby program.rb -c test.txt```の場合、```ARGV```は以下のように代入されます。

```
[0] = "-c"
[1] = "test.txt"
```

また、コマンドライン引数のうちファイル名を受け取る際、```File.open```メソッドを使用してファイルを開き、ファイルの内容を読み取ることができます。これは、ファイル操作をとても簡単にする方法です。

# 関連リンク

- [Ruby のコマンドライン引数の読み取り方](https://www.rubyguides.com/2015/05/working-with-command-line-arguments/)
- [コマンドライン引数を扱う練習問題](https://ruby-doc.com/core-3.0.0/ARGF.html#class-ARGF-label-Understanding+Command-Line+Arguments)
- [コマンドライン引数の使い方を理解する](https://www.bento.io/tutorials/command-line-arguments)
- [コマンドライン引数についての詳細な説明](https://ruby.hatenadiary.com/entry/20140520/1400535508)