---
title:                "インタラクティブシェル（REPL）の使用"
date:                  2024-01-26T04:11:33.721424-07:00
model:                 gpt-4-0125-preview
simple_title:         "インタラクティブシェル（REPL）の使用"
programming_language: "Bash"
category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## 何となぜ？
REPL は Read-Eval-Print Loop の略で、シンプルな対話型コンピュータプログラミング環境です。コーダーはこれを使って、迅速にコードの記述とテストを行い、構文を実験し、アプリケーション全体を作成して実行するためのオーバーヘッドなしに、プログラミングの概念を学ぶことができます。

## 方法：
Bashでは、ターミナル自体が基本的にREPLです。コマンドを入力すると、それを読み取り、評価し、結果を出力して、次のコマンドを待つというループになります。これはBashをREPLとして使用する例です：

```Bash
$ echo "Hello, World!"
Hello, World!
$ x=$((6 * 7))
$ echo $x
42
```

入力は `$ ` プロンプトに続き、出力は次の行に印刷されます。シンプルですよね？

## 掘り下げ
Bash、つまり Bourne Again SHell は多くのUnix系システムでデフォルトのシェルです。これは1970年代後半に構築されたオリジナルのBourneシェルへのアップグレードです。Bashは強力なスクリプティングツールですが、その対話モードではコマンドを一行ずつ実行できます。

代替案を検討する場合、Python REPL（ターミナルに`python`と入力）、Node.js（`node`と入力）、そしてIPython（拡張された対話型Pythonシェル）があります。ほとんどの言語には独自のREPL実装が存在します。

基本的に、REPLは入力（コマンドまたはコード）を解析し、実行して、結果を標準出力（あなたの画面）に返すループです。言語のインタープリタを直接使用することが多いです。このフィードバックの即時性は、学習やプロトタイピングには非常に優れています。

## 参照
- [公式GNU Bashドキュメント](https://gnu.org/software/bash/manual/bash.html)
- [Learn Shell 対話型チュートリアル](https://www.learnshell.org/)
- [IPython公式ウェブサイト](https://ipython.org/)
- [REPL.it](https://replit.com/): マルチ言語オンラインREPL（Bashだけでない！）
