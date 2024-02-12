---
title:                "テキストファイルの読み込み"
date:                  2024-01-20T17:55:12.456499-07:00
model:                 gpt-4-1106-preview
simple_title:         "テキストファイルの読み込み"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
ファイル読み込みとは、テキストファイルの内容をプログラムに取り込むことです。データ処理や設定の読み出しに使われ、どんなプログラムにも基本となる機能です。

## How to: (方法)
```Ruby
# シンプルに一行ずつ読む
File.foreach('example.txt') do |line|
  puts line
end

# 全文を一度に読む
content = File.read('example.txt')
puts content

# ファイルを開いて処理する
File.open('example.txt', 'r') do |file|
  file.each_line do |line|
    puts line
  end
end
```
サンプル出力:
```
こんにちは、世界！
こんにちは、ルビー！
```

## Deep Dive (詳細情報)
テキストファイルを読む方法は長い歴史があります。Rubyが登場する前は、C言語やPerlで行われていました。Rubyでは`IO`クラスの方法を使いますが、それには`File`クラスの方法も含まれます。`File.read`や`File.foreach`は手軽ですが、大きなファイルには`File.open`とブロックを使ってメモリを節約する方法が向いています。 

## See Also (関連情報)
- [Rubyの公式ドキュメント](https://ruby-doc.org/core-3.1.2/File.html)
- [Ruby IOクラス](https://ruby-doc.org/core-3.1.2/IO.html)
- [プログラミング初心者のためのRuby入門](https://www.ruby-lang.org/ja/documentation/quickstart/)
