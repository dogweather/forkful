---
title:                "コンピュータプログラミングにおける「コマンドライン引数の読み込み」"
html_title:           "Bash: コンピュータプログラミングにおける「コマンドライン引数の読み込み」"
simple_title:         "コンピュータプログラミングにおける「コマンドライン引数の読み込み」"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## なにそれ？なんでやるの？

コマンドライン引数とは、Bashプログラムを実行する際に、プログラムに与える情報のことです。例えば、「ls」コマンドの後ろに「-l」をつけて実行すると、ファイルの詳細情報が表示されます。プログラマーはコマンドライン引数を使うことで、プログラムをより柔軟に動作させることができます。

## 方法：

```Bash
#!/bin/bash
# コマンドライン引数を取得する例
echo "今日の日付は$1です。"
echo "私の名前は$2です。"
echo "あなたの名前は$3です。"
```

```Bash
$ bash script.sh 2021-10-01 John Jane
今日の日付は2021-10-01です。
私の名前はJohnです。
あなたの名前はJaneです。
```

## 深掘り：

ここまでの例は、コマンドライン引数が必要な場合のみを想定していますが、プログラムの実行時に入力を必要とする場合は、対話的な入力よりもコマンドライン引数を使った方が効率的です。また、プログラム内で条件分岐を使うことで、複数のコマンドライン引数を柔軟に受け入れることができます。

代わりに、環境変数を使ってプログラムを実行することもできますが、環境変数はプログラム間で共有されるため、セキュリティ上の懸念があります。

コマンドライン引数は、標準の入力方法である「標準入力」と「パイプ」に比べるとあまり使用されることはありませんが、重要なツールとして覚えておくことが大切です。

## 関連情報：

[Bashのドキュメント](https://www.gnu.org/software/bash/manual/html_node/Bash-Builtins.html#Bash-Builtins)
[コマンドライン引数と環境変数の違い](https://stackoverflow.com/questions/35006383/differences-between-environment-variables-and-command-line-arguments-in-bash)