---
title:                "新しいプロジェクトを始める"
html_title:           "C: 新しいプロジェクトを始める"
simple_title:         "新しいプロジェクトを始める"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 何となぜ？

新規プロジェクトを開始するとはプログラミングの新しい挑戦で、ゼロから何かを形作ることです。この過程を通じて、プログラマーは新しい技能を学び、創造力を発揮し、問題解決能力を高めるために行います。

## ハウツー：

次の手順を使って、新しいプロジェクトをFish Shellで開始します：
```fish 
mkdir newproject
cd newproject
touch main.fish
code main.fish
```
上記のコードは新たにディレクトリ "newproject" を作成し、その中に新しく "main.fish" ファイルを作ります。その後、そのファイルをエディタで開きます。

## ディープダイブ：

過去にはプロジェクトのために手書きのプログラムを作成することが主流でしたが、今日ではFish Shellのようなシェルコマンドはそれを容易にします。代わりに、BashやZshのような伝統的な複雑なシェルを使うこともできます。

Fish Shellでプロジェクトを始めるときの実装を理解するために、"mkdir" がディレクトリを作成し、"touch"が新しい空ファイルを作成し、"code" がエディタを開くことを理解することが重要です。これらの基本的なコマンドを組み合わせることにより、新しいプロジェクトが簡単に始まります。

## 関連情報：

Fish Shellの公式ドキュメンテーション（英語）：https://fishshell.com/docs/current/tutorial.html
BashとZshについての詳細情報（英語）：https://www.howtogeek.com/362372/what-is-zsh-and-why-should-you-use-it-instead-of-bash/