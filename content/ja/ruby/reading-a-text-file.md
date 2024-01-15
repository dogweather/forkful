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

## なぜ

テキストファイルを読むのに一番理由は、プログラミングにおいて重要なデータを扱うときに使用されるからです。例えば、CSVファイルのデータを読み込んで処理する場合などにテキストファイルを読む必要があります。

## 使い方

テキストファイルを読むには、Rubyの組み込みメソッドである `File.foreach` を使うことができます。その際、ファイルパスを引数として渡し、ブロック内で `|line|` のように行ごとにデータを取得することができます。例えば、次のようにコードを書くことができます。

```ruby
File.foreach("example.txt") do |line|
  puts line
end
```

上記のコードを実行すると、`example.txt` ファイルの中身がコンソールに出力されます。また、ファイルの内容を配列として取得することもできます。例えば、次のようにコードを書くことができます。

```ruby
lines = File.readlines("example.txt")
puts lines
```

上記のコードを実行すると、 `example.txt` ファイルの内容が配列として `lines` 変数に格納され、そのままコンソールに出力されます。

## もっと詳しく

テキストファイルを読む際には、ファイルのエンコーディングに気を付ける必要があります。特に、日本語のテキストファイルを読む場合は、UTF-8のエンコーディングを指定しておくことが重要です。また、ファイルをオープンした後は、必ずファイルを閉じる必要があります。これは、ファイルを開いたプログラムが終了しても、ファイルがまだ使用中の状態にならないようにするためです。

## 参考リンク

- [Rubyの公式ドキュメント - テキストファイルを読み込む](https://docs.ruby-lang.org/ja/2.6.0/class/File.html#I_READING-FILES)
- [オープン大学 - テキストファイルを読み込む](https://ousar.lib.okayama-u.ac.jp/4627)
- [TechAcademy Magazine - CSVファイルを読み込む](https://techacademy.jp/magazine/48462)