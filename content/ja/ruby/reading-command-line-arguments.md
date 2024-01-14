---
title:    "Ruby: コンピュータプログラミングの記事：コマンドライン引数の読み取り"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## なぜ
なぜコマンドライン引数を読み取る必要があるのでしょうか？コマンドライン引数はプログラムがユーザーからの入力を受け取り、その入力に応じて処理をさせるために使用されます。例えば、ファイルのパスやフォーマットの指定などをユーザーが指定することができます。

## 方法
コマンドライン引数を取得するには、Rubyの`ARGV`変数を使用します。`ARGV`にはユーザーが入力した引数が配列として格納されています。

```Ruby
puts ARGV # ["file/path", "format"]
```

ユーザーが入力した引数の数や特定の引数の値を取得することもできます。

```Ruby
puts ARGV.length # 2
puts ARGV[0] # "file/path"
```

## 深入り
コマンドライン引数は`$PROGRAM_NAME`という組み込み変数でも参照することができます。これは実行されているプログラムのファイル名を表します。

また、`OptionParser`を使用することでユーザーが指定する引数の数や値の形式を制限することもできます。これにより、プログラムの安全性や使いやすさを向上させることができます。

## 参考リンク
- [Rubyのコマンドライン引数について (Qiita)](https://qiita.com/wMETAw/items/d1bd1a65d16fbdd41cec)
- [Rubyドキュメント - ARGV (英語)](https://ruby-doc.org/core-2.6.3/ARGF.html)
- [OptionParserの使い方 (Qiita)](https://qiita.com/yusabana/items/5c84d0042ae9eb26f306)