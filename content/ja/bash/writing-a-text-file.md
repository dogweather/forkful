---
title:                "Bash: 「テキストファイルの作成」"
simple_title:         "「テキストファイルの作成」"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/writing-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ

プログラミングをする理由は様々ですが、テキストファイルを書くことには重要な役割があります。テキストファイルを書くことによって、コンピューターに対して特定の作業や情報を伝えることができます。

## 方法

テキストファイルを書くには、Bashプログラミングを使用します。BashはGNUプロジェクトで開発されたUnixシェルで、文字ベースのコマンドラインで操作することができます。以下は、テキストファイルを書くためのBashプログラミングの例です。

```Bash
touch sample.txt # sample.txtという名前の空のテキストファイルを作成する
echo "Hello, world!" >> sample.txt # "Hello, world!"というテキストをsample.txtに追加する
cat sample.txt # sample.txtの中身を表示する
```

上記のコードを実行すると、sample.txtに"Hello, world!"というテキストが追加され、その内容が表示されます。

## ディープダイブ

テキストファイルにはさまざまな使い方があります。プログラムの設定ファイルやデータの保存に使われることが多く、複数の行や文字列を含むことができます。また、テキストファイルを読み取ることで、プログラムの実行結果を確認することもできます。

## 参考文献

- [GNU Bash 公式ドキュメント](https://www.gnu.org/software/bash/)
- [コマンドラインチュートリアル](https://www.learnshell.org/)
- [bash-scripting-guide](https://github.com/Idnan/bash-guide)
- [Linuxコマンドライン大全](https://amzn.to/2Z61EFA)