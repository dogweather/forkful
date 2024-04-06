---
date: 2024-01-20 17:55:12.456499-07:00
description: "How to: (\u65B9\u6CD5) \u30B5\u30F3\u30D7\u30EB\u51FA\u529B."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.662076-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u8AAD\u307F\u8FBC\u307F"
weight: 22
---

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
