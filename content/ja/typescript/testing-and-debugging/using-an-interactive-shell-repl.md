---
date: 2024-01-26 04:18:48.692235-07:00
description: "\u65B9\u6CD5\uFF1A TypeScript\u306B\u306F\u72EC\u81EA\u306EREPL\u304C\
  \u4ED8\u5C5E\u3057\u3066\u3044\u307E\u305B\u3093\u3002`ts-node`\u3092\u4F7F\u7528\
  \u3057\u307E\u3057\u3087\u3046\u3002\u3053\u308C\u306FNode.js\u7528\u306ETypeScript\u5B9F\
  \u884C\u74B0\u5883\u3067\u3001REPL\u304C\u542B\u307E\u308C\u3066\u3044\u307E\u3059\
  \u3002 \u307E\u305A\u3001\u30B0\u30ED\u30FC\u30D0\u30EB\u306B\u30A4\u30F3\u30B9\u30C8\
  \u30FC\u30EB\u3057\u307E\u3059\uFF1A."
lastmod: '2024-04-05T22:37:50.056619-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A TypeScript\u306B\u306F\u72EC\u81EA\u306EREPL\u304C\u4ED8\
  \u5C5E\u3057\u3066\u3044\u307E\u305B\u3093\u3002`ts-node`\u3092\u4F7F\u7528\u3057\
  \u307E\u3057\u3087\u3046\u3002\u3053\u308C\u306FNode.js\u7528\u306ETypeScript\u5B9F\
  \u884C\u74B0\u5883\u3067\u3001REPL\u304C\u542B\u307E\u308C\u3066\u3044\u307E\u3059\
  \u3002 \u307E\u305A\u3001\u30B0\u30ED\u30FC\u30D0\u30EB\u306B\u30A4\u30F3\u30B9\u30C8\
  \u30FC\u30EB\u3057\u307E\u3059\uFF1A."
title: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\uFF08REPL\uFF09\
  \u306E\u4F7F\u7528"
weight: 34
---

## 方法：
TypeScriptには独自のREPLが付属していません。`ts-node`を使用しましょう。これはNode.js用のTypeScript実行環境で、REPLが含まれています。

まず、グローバルにインストールします：
```bash
npm install -g ts-node
```

コマンドラインで `ts-node`と入力してREPLを開始します：
```bash
ts-node
```

試してみるための簡単なスニペットです：
```TypeScript
> let message: string = 'Hello, REPL!';
> console.log(message);
Hello, REPL!
> 
```
セッションを終了するには、`Ctrl+D`を押します。

## 深掘り
歴史的に、REPLはLispのような言語で顕著であり、動的なコード評価を可能にしました。この概念は以降広がり、多くの言語での対話式コーディングの定番となりました。

TypeScriptにおいて、`ts-node`は唯一の選択肢ではありません。代替案には、WebブラウザでTypeScript Playgroundを使用するか、適切なプラグインでTypeScriptをサポートする他のNode.jsベースのREPLを利用する方法があります。

実装の面では、`ts-node`はTypeScriptのコンパイラAPIを使用して、Node.jsによって実行される前にコードをその場でトランスパイルします。これにより即時のフィードバックが得られ、セットアップの煩雑さなしにTypeScriptの最新機能を試すのに特に便利です。

覚えておくべき一点は、REPLは迅速なテストには素晴らしいものの、従来のテスト可能で、保守しやすいコードを書くことの代わりにはならないということです。それは学習と探求のためのツールであり、適切な開発実践の代替ではありません。

## 参照
- [TypeScript公式ウェブサイト](https://www.typescriptlang.org/)
- [GitHub上のts-node](https://github.com/TypeStrong/ts-node)
- [Node.js REPLドキュメント](https://nodejs.org/api/repl.html)
- [TypeScript Playground](https://www.typescriptlang.org/play)
