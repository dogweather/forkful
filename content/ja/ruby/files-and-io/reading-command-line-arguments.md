---
title:                "コマンドライン引数の読み取り"
aliases:
- /ja/ruby/reading-command-line-arguments/
date:                  2024-01-20T17:56:52.509323-07:00
model:                 gpt-4-1106-preview
simple_title:         "コマンドライン引数の読み取り"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

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
