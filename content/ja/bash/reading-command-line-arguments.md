---
title:                "Bash: 「コマンドライン引数の読み込み」"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## なぜ

 コマンドライン引数を読み込むことの重要性を説明します。コマンドライン引数は、シェルスクリプトをより柔軟にするために使用される有用なツールです。

## 使い方

コマンドライン引数を読み込む方法を、簡単なコーディング例と共に説明します。

```Bash
# 引数を表示する例
echo "第1引数：$1"
echo "第2引数：$2"
```
出力：
```
第1引数：Hello
第2引数：World
```

```Bash
# 引数の数を表示する例
echo "引数の数：$#"
```
出力：
```
引数の数：3
```

```Bash
# 引数をループ処理する例
for arg in "$@"
do
    echo "引数： $arg"
done
```
出力：
```
引数：学校
引数：仕事
引数：勉強
```

```Bash
# デフォルト値を設定する例
FILE_NAME=${1:-"default_file.txt"}
echo "ファイル名： $FILE_NAME"
```
出力：
```
ファイル名： default_file.txt
```

## ディープダイブ

コマンドライン引数をより詳しく理解するための情報を提供します。コマンドライン引数は、スクリプト内の任意の位置で指定することができ、スクリプト内で変数として使用することができます。また、引数を使ってスクリプトを実行する際に、引数には順番があります。この順番は、数値が割り当てられます。例えば、第1引数に指定した引数は$1、第2引数に指定した引数は$2となります。

また、引数にはオプションを付けることもできます。例えば、"-l"というオプションを使って引数を指定することができます。スクリプト内でオプションを認識するためには、getoptsコマンドを使用する必要があります。

## 詳細を見る

以下のリンクを参考に、より詳細な情報を確認することができます。

- [コマンドライン引数を使ったシェルスクリプトの作成方法](https://www.tutorialspoint.com/unix/shell_scripting.htm)
- [コマンドライン引数の活用方法](https://unix.stackexchange.com/questions/261244/using-arguments-with-functions-and-getopts-bash)
- [コマンドライン引数を取得する方法](https://www.geeksforgeeks.org/how-to-pass-command-line-arguments-in-shell-script/)