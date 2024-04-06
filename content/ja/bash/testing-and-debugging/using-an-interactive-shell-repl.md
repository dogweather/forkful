---
date: 2024-01-26 04:11:33.721424-07:00
description: "\u65B9\u6CD5\uFF1A Bash\u3067\u306F\u3001\u30BF\u30FC\u30DF\u30CA\u30EB\
  \u81EA\u4F53\u304C\u57FA\u672C\u7684\u306BREPL\u3067\u3059\u3002\u30B3\u30DE\u30F3\
  \u30C9\u3092\u5165\u529B\u3059\u308B\u3068\u3001\u305D\u308C\u3092\u8AAD\u307F\u53D6\
  \u308A\u3001\u8A55\u4FA1\u3057\u3001\u7D50\u679C\u3092\u51FA\u529B\u3057\u3066\u3001\
  \u6B21\u306E\u30B3\u30DE\u30F3\u30C9\u3092\u5F85\u3064\u3068\u3044\u3046\u30EB\u30FC\
  \u30D7\u306B\u306A\u308A\u307E\u3059\u3002\u3053\u308C\u306FBash\u3092REPL\u3068\
  \u3057\u3066\u4F7F\u7528\u3059\u308B\u4F8B\u3067\u3059\uFF1A."
lastmod: '2024-04-05T22:38:41.890126-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A Bash\u3067\u306F\u3001\u30BF\u30FC\u30DF\u30CA\u30EB\u81EA\
  \u4F53\u304C\u57FA\u672C\u7684\u306BREPL\u3067\u3059\u3002\u30B3\u30DE\u30F3\u30C9\
  \u3092\u5165\u529B\u3059\u308B\u3068\u3001\u305D\u308C\u3092\u8AAD\u307F\u53D6\u308A\
  \u3001\u8A55\u4FA1\u3057\u3001\u7D50\u679C\u3092\u51FA\u529B\u3057\u3066\u3001\u6B21\
  \u306E\u30B3\u30DE\u30F3\u30C9\u3092\u5F85\u3064\u3068\u3044\u3046\u30EB\u30FC\u30D7\
  \u306B\u306A\u308A\u307E\u3059\u3002\u3053\u308C\u306FBash\u3092REPL\u3068\u3057\
  \u3066\u4F7F\u7528\u3059\u308B\u4F8B\u3067\u3059\uFF1A."
title: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\uFF08REPL\uFF09\
  \u306E\u4F7F\u7528"
weight: 34
---

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
