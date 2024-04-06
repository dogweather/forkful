---
date: 2024-01-20 17:55:12.456499-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-04-05T22:50:56.742847-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) \u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u3092\u8AAD\
  \u3080\u65B9\u6CD5\u306F\u9577\u3044\u6B74\u53F2\u304C\u3042\u308A\u307E\u3059\u3002\
  Ruby\u304C\u767B\u5834\u3059\u308B\u524D\u306F\u3001C\u8A00\u8A9E\u3084Perl\u3067\
  \u884C\u308F\u308C\u3066\u3044\u307E\u3057\u305F\u3002Ruby\u3067\u306F`IO`\u30AF\
  \u30E9\u30B9\u306E\u65B9\u6CD5\u3092\u4F7F\u3044\u307E\u3059\u304C\u3001\u305D\u308C\
  \u306B\u306F`File`\u30AF\u30E9\u30B9\u306E\u65B9\u6CD5\u3082\u542B\u307E\u308C\u307E\
  \u3059\u3002`File.read`\u3084`File.foreach`\u306F\u624B\u8EFD\u3067\u3059\u304C\u3001\
  \u5927\u304D\u306A\u30D5\u30A1\u30A4\u30EB\u306B\u306F`File.open`\u3068\u30D6\u30ED\
  \u30C3\u30AF\u3092\u4F7F\u3063\u3066\u30E1\u30E2\u30EA\u3092\u7BC0\u7D04\u3059\u308B\
  \u65B9\u6CD5\u304C\u5411\u3044\u3066\u3044\u307E\u3059\u3002"
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
