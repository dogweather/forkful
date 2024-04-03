---
date: 2024-01-20 17:56:52.509323-07:00
description: "How to: (\u65B9\u6CD5) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.875929-06:00'
model: gpt-4-1106-preview
summary: .
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
