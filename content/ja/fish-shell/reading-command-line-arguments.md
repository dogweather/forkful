---
title:                "コンピューター・プログラミングに関する記事のタイトル：「コマンドライン引数の読み込み」"
html_title:           "Fish Shell: コンピューター・プログラミングに関する記事のタイトル：「コマンドライン引数の読み込み」"
simple_title:         "コンピューター・プログラミングに関する記事のタイトル：「コマンドライン引数の読み込み」"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## なに？ なんで？

コマンドライン引数を読み込むとは、プログラマーがコマンドライン上で入力された情報を処理することを指します。プログラムを実行するときに必要な情報をユーザーが入力することで、プログラムがより柔軟に動作するようになります。

##ハウツー：

```
Fish Shell```コードブロック内のコーディング例とサンプル出力。

```
例えば、以下のコードを使用してコマンドライン引数を読み込むことができます。

```
fish
#!/usr/bin/env fish

echo "こんにちは $argv"
```

```
$ fish script.fish 村田さん
こんにちは 村田さん
```

##ディープダイブ：

プログラミングにおけるコマンドライン引数の使用は、古くから存在しています。以前は、ほとんどのプログラムがコマンドライン引数を扱う方法として getopt() 関数を使用していましたが、Fish Shell ではより簡単にコマンドライン引数を処理するための独自の仕組みを提供しています。

##関連情報：

- https://fishshell.com/docs/current/tutorial.html#tut_arguments
- https://linux.die.net/man/3/getopt