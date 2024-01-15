---
title:                "新しいプロジェクトを開始する"
html_title:           "Bash: 新しいプロジェクトを開始する"
simple_title:         "新しいプロジェクトを開始する"
programming_language: "Bash"
category:             "Bash"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/starting-a-new-project.md"
---

{{< edit_this_page >}}

## なぜ始めるのか

新しいプロジェクトを始める理由は人それぞれですが、自分のアイデアを実現したい、スキルを磨きたい、あるいは楽しむためになど、様々な理由があるかと思います。

## はじめ方

Bashはカジュアルなコーディング言語としても人気があり、プロジェクトを始めるのにもぴったりです。まずはターミナルを開き、以下のコマンドを入力して始めましょう。

```Bash
mkdir myproject
cd myproject
touch index.html
```

これで新しいプロジェクトのフォルダーを作成し、index.htmlファイルを作成することができました。続いて、任意のエディターでindex.htmlを開き、HTMLコードを記述し、保存しましょう。

```Bash
nano index.html
```

保存したら、以下のコマンドを入力して、プロジェクトを実行してみましょう。

```Bash
firefox index.html
```

すると、ウェブブラウザーが立ち上がり、先ほど記述したHTMLコードが表示されるはずです。

## ディープダイブ

さらに深くBashのプロジェクトを始めることができます。例えば、以下のようなコマンドを使用して、サンプルファイルをダウンロードし、解凍することができます。

```Bash
wget https://github.com/myproject/samplefile.zip
unzip samplefile.zip
```

また、サンプルファイルを編集したい場合は、```nano```コマンドを使用して編集し、保存した後に再度```firefox```コマンドを使用して実行できます。

## See Also
- [Bash公式ドキュメンテーション](https://www.gnu.org/software/bash/manual/)
- [Codecademy: Learn Bash](https://www.codecademy.com/learn/learn-the-command-line)