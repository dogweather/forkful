---
title:                "一時ファイルの作成"
html_title:           "Bash: 一時ファイルの作成"
simple_title:         "一時ファイルの作成"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

今回は、Bashプログラミングのテクニックの一つである、一時ファイルの作成について学びましょう。

## 何をするものか?

一時ファイルとは、一時的にデータを保存するための特別なファイルです。プログラマーにとって、一時ファイルは非常に便利で、プロセス間でデータをやり取りする際や、一時的な情報を保存する際などに使用されます。

## 作り方:

以下のように、Bashコマンドを使用して一時ファイルを作成することができます。

```Bash
# テキストファイルとして一時ファイルを作成する
tmpfile=$(mktemp)

# 一時ファイルを使用する
cat <some_data> > $tmpfile

# 作業が終了したら、一時ファイルを削除する
rm $tmpfile
```

一時ファイルを作成する際、```mktemp```コマンドを使用することが一般的です。このコマンドは、一意なファイル名を自動的に生成してくれます。一時ファイルの名前には、通常以下のように```tmp.XXXXXX```という形式が使用されます。

## 詳細を掘り下げる:

一時ファイルの概念は、UNIXシステムの20世紀初頭に遡ることができます。当初は、シェルスクリプト内で使用される特別な変数であったが、後に```mktemp```コマンドが開発され、一時ファイルの作成が容易になりました。Bash以外にも、PerlやPythonなどのプログラミング言語でも一時ファイルを作成するための機能が提供されています。

また、一時ファイルを作成する別の方法として、一時ディレクトリを作成する方法もあります。これには、```mktemp -d```コマンドを使用します。この方法を使用することで、複数の一時ファイルを同じ場所に保存できるようになります。

## 関連リンク:

- [Bashの一時ファイル作成についてのドキュメンテーション](https://www.gnu.org/software/bash/manual/html_node/Creating-Temporary-Files.html)
- [mktempコマンドのドキュメンテーション](https://www.gnu.org/software/coreutils/manual/html_node/mktemp-invocation.html#mktemp-invocation)
- [一時ファイルの作成方法に関するStack Overflowの質問](https://stackoverflow.com/questions/1167746/how-to-create-a-temporary-directory-and-a-temporary-file-in-it)