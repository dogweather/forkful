---
date: 2024-01-26 01:16:33.631035-07:00
description: "\u65B9\u6CD5\uFF1A \u57FA\u672C\u7684\u306A\u96FB\u5353\u3092\u4F5C\u6210\
  \u3059\u308B\u3068\u60F3\u50CF\u3057\u3066\u307F\u3066\u304F\u3060\u3055\u3044\u3002\
  \u52A0\u7B97\u30ED\u30B8\u30C3\u30AF\u3092\u5FC5\u8981\u3068\u3059\u308B\u5834\u6240\
  \u3054\u3068\u306B\u66F8\u304F\u4EE3\u308F\u308A\u306B\u3001`add` \u95A2\u6570\u3092\
  \u4F5C\u6210\u3057\u307E\u3059\uFF1A."
lastmod: '2024-04-05T22:37:50.062282-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A \u57FA\u672C\u7684\u306A\u96FB\u5353\u3092\u4F5C\u6210\
  \u3059\u308B\u3068\u60F3\u50CF\u3057\u3066\u307F\u3066\u304F\u3060\u3055\u3044\u3002\
  \u52A0\u7B97\u30ED\u30B8\u30C3\u30AF\u3092\u5FC5\u8981\u3068\u3059\u308B\u5834\u6240\
  \u3054\u3068\u306B\u66F8\u304F\u4EE3\u308F\u308A\u306B\u3001`add` \u95A2\u6570\u3092\
  \u4F5C\u6210\u3057\u307E\u3059\uFF1A."
title: "\u30B3\u30FC\u30C9\u3092\u95A2\u6570\u306B\u6574\u7406\u3059\u308B"
weight: 18
---

## 方法：
基本的な電卓を作成すると想像してみてください。加算ロジックを必要とする場所ごとに書く代わりに、`add` 関数を作成します：

```TypeScript
function add(x: number, y: number): number {
  return x + y;
}

console.log(add(5, 7)); // サンプル出力: 12
```

次に、乗算のための関数が必要だとしましょう：

```TypeScript
function multiply(x: number, y: number): number {
  return x * y;
}

console.log(multiply(3, 4)); // サンプル出力: 12
```
関数ごとに1つのタスクに焦点を当てていることに気が付きますか？それがコードを整理することの核心です。

## 深堀り
歴史的に、プログラミング言語が進化するにつれて、関数は数学の関数から引き出され、コードを構造化する上で不可欠となりました。それらは手続き型プログラミングにおいて基本となり、オブジェクト指向および関数型プログラミングのパラダイムにて生き続けています。

代替策？関数を使わないこともできますが、それはスパゲッティタウンへの片道切符です。または、OOP（オブジェクト指向プログラミング）に進み、機能性をメソッドにパックすることもできます―これらは基本的にオブジェクトに属する関数です。

実装に関して、TypeScriptは型にこだわります。関数の入出力の型を定義することは、単なる良識ではなく、クリーンなTypeScriptコードには必須です。さらに、TypeScriptを使うことで、オーバーロード、ジェネリクス、オプショナルパラメータといった便利な機能を使って関数を強化できます。

## 関連情報
関数のスキルをレベルアップするために、これらのリソースをチェックしてください：

- [TypeScript ハンドブック – 関数](https://www.typescriptlang.org/docs/handbook/2/functions.html): TypeScript関数のためのあなたのバイブル。
- [クリーンコード JavaScript](https://github.com/ryanmcdermott/clean-code-javascript#functions): JavaScript関数にクリーンコードの原則を適用します。
- [You Don’t Know JS – スコープ & クロージャ](https://github.com/getify/You-Dont-Know-JS): JavaScriptにおけるスコープとクロージャと共に関数がどのように機能するかを把握します。
