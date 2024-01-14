---
title:    "Fish Shell: コンピュータープログラミングの記事タイトル：コマンドライン引数の読み込み"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## なぜ

コマンドライン引数を読むことに興味がある方にとって、このブログポストは有益な情報を提供することを目的としています。Fish Shellでのコマンドライン引数の使用方法を学ぶことで、より効率的にコマンドライン作業を行うことができます。

## 方法

コマンドライン引数の値を読み取りたい場合、Fish Shellの ```read``` コマンドを使用することができます。例えば、以下のようにコマンドを入力します。

```
set name "John"
read -p "Enter your age: " age
```

このコマンドでは、```name``` という変数に ```John``` という値が、```age```という変数にユーザーから入力された値が保存されます。入力時には、```Enter your age: ```というプロンプトが表示されます。

もし、デフォルトの値を設定したい場合は、以下のようにコマンドを入力します。

```
set name "John"
read -p "Enter your favorite color: " -i "blue" color
```

この場合、ユーザーが入力しなかった場合には、デフォルト値として ```blue``` が設定されます。

特定の型に変換したい場合には、```-E```オプションを使用することができます。例えば、数字の入力を求める場合は、以下のようにコマンドを入力します。

```
read -p "Enter your age: " -E age
```

また、複数の値を同時に読み込む場合は、以下のようにコマンドを入力します。

```
read -p "Enter your name and age: " name age
```

これにより、入力された値がそれぞれ ```name``` と ```age``` の変数に保存されます。

## 深堀り

Fish Shellの ```read```コマンドでは、様々なオプションを使用することができます。例えば、```-q```オプションを使用すれば、特定の条件を満たした場合にのみ、プロンプトの表示をスキップすることができます。また、```-s```オプションを使用すれば、入力された値を表示することなく、パスワードのように隠された形で保存することができます。

さらに、深く知りたい方のために、以下のリンクをご参考ください。

## 詳しくはこちらを参照してください

- [Fish Shell公式ドキュメント - read](https://fishshell.com/docs/current/cmds/read.html)
- [Bash Shellとの対比 - Bash vs Fish](https://blog.benestudio.co/bash-vs-fish-939f9530c441)
- [コマンドライン引数の取得方法についての詳細な解説（英語）](https://www.linuxjournal.com/content/bash-command-substitution-everything-you-ever-wanted-know-bash-arrays)
- [コマンドライン引数を使用する際のコツ（英語）](https://www.digitalocean.com/community/tutorials/how-to-read-and-set-stuff-in-unix)