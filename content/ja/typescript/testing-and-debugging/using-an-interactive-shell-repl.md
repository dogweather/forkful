---
date: 2024-01-26 04:18:48.692235-07:00
description: "Read-Eval-Print-Loop\uFF08REPL\uFF09\u306F\u3001\u30E6\u30FC\u30B6\u30FC\
  \u306E\u5165\u529B\u3092\u53D7\u3051\u53D6\u308A\u3001\u5B9F\u884C\u3057\u3001\u7D50\
  \u679C\u3092\u30E6\u30FC\u30B6\u30FC\u306B\u8FD4\u3059\u30D7\u30ED\u30B0\u30E9\u30DF\
  \u30F3\u30B0\u74B0\u5883\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\
  REPL\u3092\u4F7F\u7528\u3057\u3066\u3001\u30B3\u30FC\u30C9\u30B9\u30CB\u30DA\u30C3\
  \u30C8\u3092\u7D20\u65E9\u304F\u8A66\u3057\u305F\u308A\u3001\u30C7\u30D0\u30C3\u30B0\
  \u3057\u305F\u308A\u3001\u30D5\u30EB\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\
  \u3092\u4F5C\u6210\u3059\u308B\u7169\u96D1\u3055\u306A\u3057\u306B\u65B0\u3057\u3044\
  \u8A00\u8A9E\u6A5F\u80FD\u3092\u5B66\u3093\u3060\u308A\u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.760269-06:00'
model: gpt-4-0125-preview
summary: "Read-Eval-Print-Loop\uFF08REPL\uFF09\u306F\u3001\u30E6\u30FC\u30B6\u30FC\
  \u306E\u5165\u529B\u3092\u53D7\u3051\u53D6\u308A\u3001\u5B9F\u884C\u3057\u3001\u7D50\
  \u679C\u3092\u30E6\u30FC\u30B6\u30FC\u306B\u8FD4\u3059\u30D7\u30ED\u30B0\u30E9\u30DF\
  \u30F3\u30B0\u74B0\u5883\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\
  REPL\u3092\u4F7F\u7528\u3057\u3066\u3001\u30B3\u30FC\u30C9\u30B9\u30CB\u30DA\u30C3\
  \u30C8\u3092\u7D20\u65E9\u304F\u8A66\u3057\u305F\u308A\u3001\u30C7\u30D0\u30C3\u30B0\
  \u3057\u305F\u308A\u3001\u30D5\u30EB\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\
  \u3092\u4F5C\u6210\u3059\u308B\u7169\u96D1\u3055\u306A\u3057\u306B\u65B0\u3057\u3044\
  \u8A00\u8A9E\u6A5F\u80FD\u3092\u5B66\u3093\u3060\u308A\u3057\u307E\u3059\u3002."
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
