---
title:                "コンピュータプログラミング：コマンドライン引数の読み取り"
html_title:           "Ruby: コンピュータプログラミング：コマンドライン引数の読み取り"
simple_title:         "コンピュータプログラミング：コマンドライン引数の読み取り"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## コマンドライン引数の読み込みとは？

コマンドライン引数の読み込みとは、プログラムが実行される際に与えられる引数を、プログラム内で扱うために受け取ることです。プログラマーは、コマンドライン引数を読み込むことで、柔軟性のあるプログラムを作成することができます。

## 手順：

```ruby
# コマンドライン引数を読み込む
ARGV.each do |args|
  puts "引数: #{args}"
end
```

上記のコードでは、プログラムを実行する際に与えられた引数を、全て受け取って表示することができます。例えば、「ruby test.rb hello world」というコマンドを実行した場合、「引数: hello」と「引数: world」という出力が得られます。

## 詳細について：

コマンドライン引数の読み込みは、プログラミング言語によって実装方法が異なります。また、代替手段として、環境変数や設定ファイルから読み込む方法もあります。コマンドライン引数の読み込みは、コマンドラインインタフェース（CLI）を持つツールやアプリケーションを作成する際に欠かせない機能です。

## 関連情報：

- [Rubyのコマンドライン引数の受け取り方](https://www.techpit.jp/courses/32/curriculums/33/sections/182/parts/627)
- [プログラム引数の利用方法 (Rubyガイド)](https://ruby-lang.co/programargs/)
- [コマンドライン引数の基本 (ドットインストール)](https://dotinstall.com/lessons/basic_cli_ruby_v2)