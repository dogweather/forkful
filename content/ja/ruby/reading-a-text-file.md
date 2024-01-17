---
title:                "テキストファイルの読み込み"
html_title:           "Ruby: テキストファイルの読み込み"
simple_title:         "テキストファイルの読み込み"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 何？なぜ？

テキストファイルを読むことは、プログラマーにとって重要な作業です。テキストファイルを読むことは、プログラムの一部であるデータの入力手段となります。プログラマーは、テキストファイルを読んでデータを取得し、プログラムの中で処理することができます。

## 方法：

```Ruby
File.read("file.txt")            # ファイル全体を読み込みます。
File.readlines("file.txt")       # 1行ずつ読み込み、配列として返します。
File.open("file.txt", "r") do |file|
  file.each_line do |line|       # ファイルを1行ずつ読み込み、ブロック内で処理します。
    puts line.chomp
  end
end
```

```file.txt``` が以下のように与えられた場合、

```
こんにちは、世界！
Hello, World!
```

上記のコードは、以下のような出力を返します。

```
こんにちは、世界！
Hello, World!
```

「```chomp```」を使うことで、改行文字を取り除くことができます。

## 詳細：

テキストファイルを読み込む方法は、プログラム言語によって異なります。しかし、ほとんどのプログラミング言語では、ファイルを開き、ファイルを読み込むメソッドを使用することで簡単にファイルを読むことができます。また、テキストファイルを書き込むことも可能です。

代替手段としては、データベースやAPIを使用することができます。しかし、テキストファイルを使用することで、よりシンプルで効率的なデータの管理ができる場合もあります。

テキストファイルを読み込む方法は、プログラミング言語の学習において重要なスキルの一つです。様々なプロジェクトでテキストファイルを使用することがあるため、覚えておくことが役に立つでしょう。

## 関連リンク：

- [Rubyドキュメント](https://ruby-doc.org/core-3.0.0/File.html)
- [プログラミング初心者のためのテキストファイルの読み書き方法](https://railsguides.jp/working_with_javascript_in_rails.html#%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB%E3%81%AE%E8%AA%AD%E3%81%BF%E8%BE%BC%E3%81%BF)