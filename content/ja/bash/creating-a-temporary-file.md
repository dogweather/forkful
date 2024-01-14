---
title:                "Bash: 一時的なファイルを作成する"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## なぜ

一時ファイルを作成することに取り組む理由はいくつかあります。一時ファイルを使用することで、データの一時的な保存やプログラムの実行に必要なファイルの作成が容易になります。また、一時ファイルを使用すると、データを簡単に削除できるため、セキュリティの観点からも有用です。

## 作り方

一時ファイルを作成するためには、次のようなBashコードを使用します。

```Bash
#!/bin/bash

# 一時ファイルを作成する
temp_file=$(mktemp) 
```

上記のコードでは、mktempコマンドを使用して一時ファイルを作成し、そのファイル名を変数に保存しています。また、作成したファイルを使用した後は、次のように削除することができます。

```Bash
# 一時ファイルを削除する
rm $temp_file
```

## 深く掘り下げる

一時ファイルを作成する方法には、さまざまなオプションがあります。例えば、次のオプションを使用することで、ファイル名のプレフィックスやサフィックス、ディレクトリの指定などを行うことができます。

```Bash
# プレフィックスを指定して一時ファイルを作成する
temp_file=$(mktemp example_XXXX)

# ディレクトリを指定して一時ファイルを作成する
temp_file=$(mktemp -d /tmp/example)

# サフィックスを指定して一時ディレクトリを作成する
temp_dir=$(mktemp -d -t example_)
```

また、一時ファイルを作成する際には、安全性を考慮する必要があります。一時ファイルを作成する前に、ファイルの書き込み権限をチェックすることや、ランダムなファイル名を使用することが重要です。

## 参考リンク

- [mktempコマンドのマニュアル](https://linuxjm.osdn.jp/html/GNU_coreutils/man1/mktemp.1.html)
- [一時ファイルを作成する方法](https://www.shellhacks.com/create-temporary-file-in-bash-script/)
- [一時ファイルの作成と削除の安全性について](https://www.linux.com/tutorials/working-temporary-files-bash/)
- [一時ファイルを使用する際の注意点](https://www.cyberciti.biz/faq/create-files-in-linux-using-the-temporary-file-utility-mktemp/)