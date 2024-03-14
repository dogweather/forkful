---
date: 2024-01-26 04:11:33.721424-07:00
description: "REPL \u306F Read-Eval-Print Loop \u306E\u7565\u3067\u3001\u30B7\u30F3\
  \u30D7\u30EB\u306A\u5BFE\u8A71\u578B\u30B3\u30F3\u30D4\u30E5\u30FC\u30BF\u30D7\u30ED\
  \u30B0\u30E9\u30DF\u30F3\u30B0\u74B0\u5883\u3067\u3059\u3002\u30B3\u30FC\u30C0\u30FC\
  \u306F\u3053\u308C\u3092\u4F7F\u3063\u3066\u3001\u8FC5\u901F\u306B\u30B3\u30FC\u30C9\
  \u306E\u8A18\u8FF0\u3068\u30C6\u30B9\u30C8\u3092\u884C\u3044\u3001\u69CB\u6587\u3092\
  \u5B9F\u9A13\u3057\u3001\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u5168\u4F53\
  \u3092\u4F5C\u6210\u3057\u3066\u5B9F\u884C\u3059\u308B\u305F\u3081\u306E\u30AA\u30FC\
  \u30D0\u30FC\u30D8\u30C3\u30C9\u306A\u3057\u306B\u3001\u30D7\u30ED\u30B0\u30E9\u30DF\
  \u30F3\u30B0\u306E\u6982\u5FF5\u3092\u5B66\u3076\u3053\u3068\u304C\u3067\u304D\u307E\
  \u3059\u3002"
lastmod: '2024-03-13T22:44:42.372909-06:00'
model: gpt-4-0125-preview
summary: "REPL \u306F Read-Eval-Print Loop \u306E\u7565\u3067\u3001\u30B7\u30F3\u30D7\
  \u30EB\u306A\u5BFE\u8A71\u578B\u30B3\u30F3\u30D4\u30E5\u30FC\u30BF\u30D7\u30ED\u30B0\
  \u30E9\u30DF\u30F3\u30B0\u74B0\u5883\u3067\u3059\u3002\u30B3\u30FC\u30C0\u30FC\u306F\
  \u3053\u308C\u3092\u4F7F\u3063\u3066\u3001\u8FC5\u901F\u306B\u30B3\u30FC\u30C9\u306E\
  \u8A18\u8FF0\u3068\u30C6\u30B9\u30C8\u3092\u884C\u3044\u3001\u69CB\u6587\u3092\u5B9F\
  \u9A13\u3057\u3001\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u5168\u4F53\u3092\
  \u4F5C\u6210\u3057\u3066\u5B9F\u884C\u3059\u308B\u305F\u3081\u306E\u30AA\u30FC\u30D0\
  \u30FC\u30D8\u30C3\u30C9\u306A\u3057\u306B\u3001\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\
  \u30B0\u306E\u6982\u5FF5\u3092\u5B66\u3076\u3053\u3068\u304C\u3067\u304D\u307E\u3059\
  \u3002"
title: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\uFF08REPL\uFF09\
  \u306E\u4F7F\u7528"
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
