---
title:                "コンピュータプログラミングの記事タイトル：コマンドライン引数の読み込み"
html_title:           "Ruby: コンピュータプログラミングの記事タイトル：コマンドライン引数の読み込み"
simple_title:         "コンピュータプログラミングの記事タイトル：コマンドライン引数の読み込み"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## なぜ読むのか

コマンドライン引数を読むことは、Rubyプログラミングにおける重要なスキルです。コマンドライン引数を読むことで、ユーザーがプログラムに対して動作を指定することができるようになります。

## 読み方

コマンドライン引数を読むには、ARGV配列を使用します。この配列には、プログラムに渡されたコマンドライン引数が格納されています。

```Ruby
# ARGV配列のインデックス0番目は、実行したプログラムのパスが格納されています
puts ARGV[0]

# コマンドライン引数が複数ある場合は、それぞれの引数を使用することができます
puts ARGV[1]
puts ARGV[2]
```

例えば、以下のようにプログラムを実行した場合、

```sh
ruby my_program.rb argument1 argument2 argument3
```

次のように出力されます。

```
argument1
argument2
argument3
```

## 深堀り

コマンドライン引数は、プログラムを実行する際にオプションやパラメータを指定するために使用されます。プログラム内でARGV配列を使用することで、ユーザーが指定したオプションやパラメータを処理することができます。

しかし、コマンドライン引数を処理する際には、入力値の検証やエラーハンドリングなどを適切に行う必要があります。また、ARGV配列に格納されるデータ型は全て文字列であるため、適切に変換する必要がある場合もあります。

## おわりに

引数を読むことは、Rubyプログラミングにおいて非常に重要なスキルです。コマンドライン引数を読むことで、プログラムをより柔軟に実行することができるようになります。是非、実際に手を動かして練習してみてください。

## 関連リンク

- [Rubyの公式ドキュメント](https://www.ruby-lang.org/ja/documentation/)
- [第一引数から取得する Ruby の ARGV の意味と使い方](https://qiita.com/tbpgr/items/989c6badefff69377da7)
- [Ruby の ARGV の扱い方](https://qiita.com/ryoppy/items/9a8ae8c8e8d84bf39e16)