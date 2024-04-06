---
date: 2024-01-20 17:56:52.509323-07:00
description: "How to: (\u65B9\u6CD5) \u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\
  \u6570\u306F1980\u5E74\u4EE3\u304B\u3089Unix\u30C4\u30FC\u30EB\u306B\u304A\u3051\
  \u308B\u6A19\u6E96\u6A5F\u80FD\u3002`ARGV`\u306FRuby\u306B\u304A\u3051\u308B\u30B0\
  \u30ED\u30FC\u30D0\u30EB\u5909\u6570\u3067\u3001\u5165\u529B\u3055\u308C\u305F\u5F15\
  \u6570\u306E\u914D\u5217\u3092\u6301\u3063\u3066\u308B\u3002`ARGV`\u914D\u5217\u304B\
  \u3089\u60C5\u5831\u3092\u53D6\u308A\u51FA\u3059\u306E\u306F\u7C21\u5358\u3067\u76F4\
  \u611F\u7684\u3002 \u4ED6\u306Eoption-parser\u30E9\u30A4\u30D6\u30E9\u30EA\u3068\
  \u306E\u6BD4\u8F03\uFF1A - `optparse`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.659643-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) \u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u306F\
  1980\u5E74\u4EE3\u304B\u3089Unix\u30C4\u30FC\u30EB\u306B\u304A\u3051\u308B\u6A19\
  \u6E96\u6A5F\u80FD\u3002`ARGV`\u306FRuby\u306B\u304A\u3051\u308B\u30B0\u30ED\u30FC\
  \u30D0\u30EB\u5909\u6570\u3067\u3001\u5165\u529B\u3055\u308C\u305F\u5F15\u6570\u306E\
  \u914D\u5217\u3092\u6301\u3063\u3066\u308B\u3002`ARGV`\u914D\u5217\u304B\u3089\u60C5\
  \u5831\u3092\u53D6\u308A\u51FA\u3059\u306E\u306F\u7C21\u5358\u3067\u76F4\u611F\u7684\
  ."
title: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u306E\u8AAD\u307F\u53D6\
  \u308A"
weight: 23
---

## How to: (方法)
```Ruby
# このシンプルなコードはコマンドラインから引数を受け取って表示する。
ARGV.each_with_index do |value, index|
  puts "引数#{index}: #{value}"
end 
```

実行例：
```Shell
$ ruby your_script.rb こん にちは 世界
引数0: こん
引数1: にちは
引数2: 世界
```

## Deep Dive (深掘り)
コマンドライン引数は1980年代からUnixツールにおける標準機能。`ARGV`はRubyにおけるグローバル変数で、入力された引数の配列を持ってる。`ARGV`配列から情報を取り出すのは簡単で直感的。

他のoption-parserライブラリとの比較：
- `optparse` 標準ライブラリ。オプション解析用。
- `thor` より複雑なCLIアプリを作る時に。

実装詳細：
- `ARGV`は変更可能。引数を削除したい場合、`shift`メソッドが使える。
- セキュリティ: 不正な入力には気をつけて。サニタイズが必要な場合も。

## See Also (関連情報)
- [Pickaxe Book](https://ruby-doc.com/docs/ProgrammingRuby/) - Rubyの根本を学べる。
- [Ruby ARGV documentation](https://ruby-doc.org/core-2.7.0/ARGF.html) - `ARGV`の公式ドキュメント。
- [Optparse documentation](https://ruby-doc.org/stdlib-2.7.0/libdoc/optparse/rdoc/OptionParser.html) - オプション解析についての詳細。
