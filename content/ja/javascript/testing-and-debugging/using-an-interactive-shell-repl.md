---
date: 2024-01-26 04:15:54.366658-07:00
description: "\u4F7F\u3044\u65B9\uFF1A Node.js\u306B\u306F\u3001\u30BF\u30FC\u30DF\
  \u30CA\u30EB\u7D4C\u7531\u3067\u30A2\u30AF\u30BB\u30B9\u3067\u304D\u308BREPL\u304C\
  \u4ED8\u5C5E\u3057\u3066\u3044\u307E\u3059\u3002\u305D\u308C\u3092\u958B\u3051\u3070\
  \u3001\u3059\u3050\u306B\u59CB\u3081\u3089\u308C\u307E\u3059\u3002\u307B\u3089\u3001\
  \u3053\u3093\u306A\u611F\u3058\u3067\u3059\uFF1A."
lastmod: '2024-03-13T22:44:42.680179-06:00'
model: gpt-4-0125-preview
summary: "Node.js\u306B\u306F\u3001\u30BF\u30FC\u30DF\u30CA\u30EB\u7D4C\u7531\u3067\
  \u30A2\u30AF\u30BB\u30B9\u3067\u304D\u308BREPL\u304C\u4ED8\u5C5E\u3057\u3066\u3044\
  \u307E\u3059\u3002\u305D\u308C\u3092\u958B\u3051\u3070\u3001\u3059\u3050\u306B\u59CB\
  \u3081\u3089\u308C\u307E\u3059\u3002\u307B\u3089\u3001\u3053\u3093\u306A\u611F\u3058\
  \u3067\u3059\uFF1A."
title: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\uFF08REPL\uFF09\
  \u306E\u4F7F\u7528"
weight: 34
---

## 使い方：
Node.jsには、ターミナル経由でアクセスできるREPLが付属しています。それを開けば、すぐに始められます。ほら、こんな感じです：

```javascript
$ node
> let sum = (a, b) => a + b;
undefined
> sum(5, 10);
15
> .exit
```

簡単でしょう？変数を定義したり、関数を実行したり、ループを走らせたりできます。終わったら、`.exit`で現実世界に戻ります。

## 深掘り
REPLは1960年代から存在しており、LISPがこの概念を開拓しました。そのアイデアは、プログラマーに即時フィードバックを提供することです。代替手段としては？Node.jsのREPLの他にも、Chrome DevToolsのようなブラウザベースのコンソール、JSFiddleのようなオンラインのサンドボックス、またはVSCodeのようなインタラクティブなプレイグラウンドを持つ完全なIDEなどがあります。

内部的に、REPLのワークフローは通常：
1. 入力を読み取る
2. コードをコンパイルして実行する
3. 出力を出力する
4. 戻ってループする

これはシンプルですが効果的なサイクルであり、インタラクティブなコーディングに大きな影響を与えています。

## 参照
- [Node.jsのREPLドキュメント](https://nodejs.org/api/repl.html)
- [MozillaのREPL上のJavaScriptモジュール入門](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Modules)
- [JSFiddle](https://jsfiddle.net/)
