---
date: 2024-01-20 17:56:52.509323-07:00
description: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u3092\u8AAD\u3080\
  \u3063\u3066\uFF1F\u305D\u308C\u306F\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u304C\u5B9F\
  \u884C\u6642\u306B\u30E6\u30FC\u30B6\u30FC\u304B\u3089\u8FFD\u52A0\u30C7\u30FC\u30BF\
  \u3092\u53D7\u3051\u53D6\u308B\u65B9\u6CD5\u3055\u3002\u306A\u305C\u4F7F\u3046\u306E\
  \u304B\uFF1F\u8D77\u52D5\u306E\u969B\u306B\u52D5\u7684\u306B\u30D7\u30ED\u30B0\u30E9\
  \u30E0\u3092\u8A2D\u5B9A\u3057\u305F\u308A\u3001\u7279\u5B9A\u306E\u30BF\u30B9\u30AF\
  \u306B\u5408\u308F\u305B\u3066\u632F\u308B\u821E\u3044\u3092\u5909\u3048\u305F\u308A\
  \u3059\u308B\u305F\u3081\u3060\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.875929-06:00'
model: gpt-4-1106-preview
summary: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u3092\u8AAD\u3080\
  \u3063\u3066\uFF1F\u305D\u308C\u306F\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u304C\u5B9F\
  \u884C\u6642\u306B\u30E6\u30FC\u30B6\u30FC\u304B\u3089\u8FFD\u52A0\u30C7\u30FC\u30BF\
  \u3092\u53D7\u3051\u53D6\u308B\u65B9\u6CD5\u3055\u3002\u306A\u305C\u4F7F\u3046\u306E\
  \u304B\uFF1F\u8D77\u52D5\u306E\u969B\u306B\u52D5\u7684\u306B\u30D7\u30ED\u30B0\u30E9\
  \u30E0\u3092\u8A2D\u5B9A\u3057\u305F\u308A\u3001\u7279\u5B9A\u306E\u30BF\u30B9\u30AF\
  \u306B\u5408\u308F\u305B\u3066\u632F\u308B\u821E\u3044\u3092\u5909\u3048\u305F\u308A\
  \u3059\u308B\u305F\u3081\u3060\u3002."
title: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u306E\u8AAD\u307F\u53D6\
  \u308A"
weight: 23
---

## What & Why? (何となぜ？)
コマンドライン引数を読むって？それは、プログラムが実行時にユーザーから追加データを受け取る方法さ。なぜ使うのか？起動の際に動的にプログラムを設定したり、特定のタスクに合わせて振る舞いを変えたりするためだ。

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
