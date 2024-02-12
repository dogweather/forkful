---
title:                "インタラクティブシェル（REPL）の使用"
aliases:
- /ja/javascript/using-an-interactive-shell-repl/
date:                  2024-01-26T04:15:54.366658-07:00
model:                 gpt-4-0125-preview
simple_title:         "インタラクティブシェル（REPL）の使用"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/using-an-interactive-shell-repl.md"
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
