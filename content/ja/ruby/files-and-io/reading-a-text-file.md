---
date: 2024-01-20 17:55:12.456499-07:00
description: "\u30D5\u30A1\u30A4\u30EB\u8AAD\u307F\u8FBC\u307F\u3068\u306F\u3001\u30C6\
  \u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u5185\u5BB9\u3092\u30D7\u30ED\u30B0\
  \u30E9\u30E0\u306B\u53D6\u308A\u8FBC\u3080\u3053\u3068\u3067\u3059\u3002\u30C7\u30FC\
  \u30BF\u51E6\u7406\u3084\u8A2D\u5B9A\u306E\u8AAD\u307F\u51FA\u3057\u306B\u4F7F\u308F\
  \u308C\u3001\u3069\u3093\u306A\u30D7\u30ED\u30B0\u30E9\u30E0\u306B\u3082\u57FA\u672C\
  \u3068\u306A\u308B\u6A5F\u80FD\u3067\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-11T00:14:16.428340-06:00'
model: gpt-4-1106-preview
summary: "\u30D5\u30A1\u30A4\u30EB\u8AAD\u307F\u8FBC\u307F\u3068\u306F\u3001\u30C6\
  \u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u5185\u5BB9\u3092\u30D7\u30ED\u30B0\
  \u30E9\u30E0\u306B\u53D6\u308A\u8FBC\u3080\u3053\u3068\u3067\u3059\u3002\u30C7\u30FC\
  \u30BF\u51E6\u7406\u3084\u8A2D\u5B9A\u306E\u8AAD\u307F\u51FA\u3057\u306B\u4F7F\u308F\
  \u308C\u3001\u3069\u3093\u306A\u30D7\u30ED\u30B0\u30E9\u30E0\u306B\u3082\u57FA\u672C\
  \u3068\u306A\u308B\u6A5F\u80FD\u3067\u3059\u3002"
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u8AAD\u307F\u8FBC\u307F"
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
