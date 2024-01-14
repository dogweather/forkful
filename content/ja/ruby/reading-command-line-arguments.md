---
title:                "Ruby: コンピュータプログラミングにおける「コマンドライン引数の読み取り」"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# なぜ

コマンドライン引数を読むのに *なぜ* 読者が参加する必要があるのか、についての 1-2 文字の説明です。

コマンドライン引数を読むことは、よりオプションを使ってプログラムを実行したい場合や、ユーザーからの入力を受け入れたい場合に役立ちます。また、複数のファイルを同時に処理する場合や、特定の処理を行うために必要な情報をプログラムに提供することができます。

# 方法

コマンドライン引数を読むためには、プログラムの引数として `ARGV` を使用します。`ARGV` は Ruby の特別なグローバル変数であり、コマンドラインに渡された引数が配列として格納されています。

```Ruby
input = ARGV[0]
puts "入力された引数は #{input} です。"
```

上記の例では、最初の引数を `ARGV` から取得し、それを文字列の一部として出力しています。

出力:

```
$ ruby read_argv.rb hello
入力された引数は hello です。
```

# 詳細を掘り下げる

コマンドライン引数をより詳しく学ぶには、Ruby のドキュメントを見ることができます。また、`OptionParser` クラスを使用して、より複雑なコマンドライン引数を処理することもできます。

さらに、ARGV を使ってプログラムに入力を提供する方法だけでなく、環境変数や標準入力からのデータを取得する方法もあります。Ruby には様々な方法でプログラムから入力を受け取る機能が用意されているため、使いやすい方法を選択することができます。

# はてしなく続く

コマンドライン引数の読み方は、プログラミングにおいて非常に重要な機能の一つです。これまでの例では簡単な入力を扱いましたが、実際にはより複雑な引数を扱う必要が出てくるでしょう。そのためには、より詳細な学習や実践が必要になりますが、それによってプログラムの機能性が向上することができます。

# 関連リンク

- [Ruby ドキュメント - `ARGV`](https://docs.ruby-lang.org/ja/latest/class/ARGV.html)
- [Ruby ドキュメント - `OptionParser`](https://docs.ruby-lang.org/ja/latest/class/OptionParser.html)
- [Ruby チュートリアル - コマンドライン引数を扱う](https://www.ruby-lang.org/ja/documentation/tutorials/quickstart/2/)