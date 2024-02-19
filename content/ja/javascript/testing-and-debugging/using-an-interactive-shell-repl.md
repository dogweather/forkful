---
aliases:
- /ja/javascript/using-an-interactive-shell-repl/
date: 2024-01-26 04:15:54.366658-07:00
description: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30FB\u30B7\u30A7\u30EB\
  \u3084REPL\uFF08Read-Eval-Print Loop\uFF09\u3067\u306F\u3001\u30B3\u30FC\u30C9\u3092\
  \u305D\u306E\u5834\u3067\u5B9F\u884C\u3067\u304D\u3001\u95A2\u6570\u3084\u30A2\u30EB\
  \u30B4\u30EA\u30BA\u30E0\u3092\u30C6\u30B9\u30C8\u3057\u305F\u308A\u3001\u30A2\u30A4\
  \u30C7\u30A2\u3092\u3044\u3058\u3063\u305F\u308A\u3067\u304D\u307E\u3059\u3002\u3053\
  \u308C\u3089\u306F\u30B3\u30FC\u30C7\u30A3\u30F3\u30B0\u306E\u30B9\u30AF\u30E9\u30C3\
  \u30C1\u30D1\u30C3\u30C9\u3067\u3042\u308A\u3001\u5B8C\u5168\u306A\u958B\u767A\u74B0\
  \u5883\u3092\u8A2D\u5B9A\u3059\u308B\u3053\u3068\u306A\u304F\u3001\u7D20\u65E9\u304F\
  \u624B\u8EFD\u306B\u4F7F\u3048\u307E\u3059\u3002"
lastmod: 2024-02-18 23:08:55.270974
model: gpt-4-0125-preview
summary: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30FB\u30B7\u30A7\u30EB\
  \u3084REPL\uFF08Read-Eval-Print Loop\uFF09\u3067\u306F\u3001\u30B3\u30FC\u30C9\u3092\
  \u305D\u306E\u5834\u3067\u5B9F\u884C\u3067\u304D\u3001\u95A2\u6570\u3084\u30A2\u30EB\
  \u30B4\u30EA\u30BA\u30E0\u3092\u30C6\u30B9\u30C8\u3057\u305F\u308A\u3001\u30A2\u30A4\
  \u30C7\u30A2\u3092\u3044\u3058\u3063\u305F\u308A\u3067\u304D\u307E\u3059\u3002\u3053\
  \u308C\u3089\u306F\u30B3\u30FC\u30C7\u30A3\u30F3\u30B0\u306E\u30B9\u30AF\u30E9\u30C3\
  \u30C1\u30D1\u30C3\u30C9\u3067\u3042\u308A\u3001\u5B8C\u5168\u306A\u958B\u767A\u74B0\
  \u5883\u3092\u8A2D\u5B9A\u3059\u308B\u3053\u3068\u306A\u304F\u3001\u7D20\u65E9\u304F\
  \u624B\u8EFD\u306B\u4F7F\u3048\u307E\u3059\u3002"
title: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\uFF08REPL\uFF09\
  \u306E\u4F7F\u7528"
---

{{< edit_this_page >}}

## 何となぜ？
インタラクティブ・シェルやREPL（Read-Eval-Print Loop）では、コードをその場で実行でき、関数やアルゴリズムをテストしたり、アイデアをいじったりできます。これらはコーディングのスクラッチパッドであり、完全な開発環境を設定することなく、素早く手軽に使えます。

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
