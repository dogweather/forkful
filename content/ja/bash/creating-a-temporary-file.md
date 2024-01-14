---
title:                "Bash: 一時ファイルの作成"
simple_title:         "一時ファイルの作成"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## なぜ

プログラミングをする人なら、一時ファイル作成について知っておくことが重要です。一時ファイルは、プログラムにとって重要な情報を保管する際や、特定のタスクを実行する際に必要になります。

## 作り方

Bashを使用して一時ファイルを作成する方法はいくつかありますが、最も一般的な方法は以下の通りです。

```Bash
# ランダムなファイル名で一時ファイルを作成
tempfile=$(mktemp)

# 作成したファイルにデータを書き込む
echo "これは一時ファイルです" > $tempfile

# ファイルを読み込んで出力する
cat $tempfile
```

実行すると、下記のような結果が得られます。

```Bash
これは一時ファイルです
```

また、一時ファイルには有用なオプションもあります。例えば、一時ファイルを作成せずに直接ファイルにデータを書き込むことができます。

```Bash
# 一時ファイルを作成せずに直接ファイルにデータを書き込む
echo "このデータは一時ファイルではなく、直接ファイルに書き込まれます" > tempfile.txt
```

## 詳細を調べる

一時ファイルを作成する際、このファイルがどこに保存されるのか知ることも重要です。一時ファイルは通常、`/tmp`ディレクトリ内に保存されますが、環境変数を設定することで保存先を変更することもできます。

また、一時ファイルを作成する際に使用される`mktemp`コマンドのオプションも重要です。`-u`オプションを使用すると、ファイルの作成だけを行ってファイル名を出力することができます。`-d`オプションを使用すると、一時ディレクトリを作成することができます。

## 参考

- [Bashで一時ファイルを作成する方法](https://www.lifewire.com/create-temporary-files-bash-scripts-2200578)
- [Linuxの一時ファイルの作成](https://www.pluralsight.com/blog/operating-systems/linux-temporary-files)
- [Bashのmktempコマンドのドキュメント](https://linux.die.net/man/1/mktemp)

## 関連情報を見る