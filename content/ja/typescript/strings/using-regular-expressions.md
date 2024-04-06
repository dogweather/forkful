---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:33.610742-07:00
description: "\u3069\u306E\u3088\u3046\u306B\u3057\u3066\uFF1A TypeScript\u306B\u5165\
  \u3063\u3066\u307F\u3066\u3001\u4E00\u822C\u7684\u306A\u30BF\u30B9\u30AF\u306B\u5BFE\
  \u3057\u3066regex\u304C\u3069\u306E\u3088\u3046\u306B\u4F7F\u7528\u3055\u308C\u3066\
  \u3044\u308B\u304B\u898B\u3066\u307F\u307E\u3057\u3087\u3046\u3002"
lastmod: '2024-04-05T22:37:50.044016-06:00'
model: gpt-4-0125-preview
summary: "\u3069\u306E\u3088\u3046\u306B\u3057\u3066\uFF1A TypeScript\u306B\u5165\u3063\
  \u3066\u307F\u3066\u3001\u4E00\u822C\u7684\u306A\u30BF\u30B9\u30AF\u306B\u5BFE\u3057\
  \u3066regex\u304C\u3069\u306E\u3088\u3046\u306B\u4F7F\u7528\u3055\u308C\u3066\u3044\
  \u308B\u304B\u898B\u3066\u307F\u307E\u3057\u3087\u3046\u3002"
title: "\u6B63\u898F\u8868\u73FE\u306E\u4F7F\u7528"
weight: 11
---

## どのようにして：
TypeScriptに入ってみて、一般的なタスクに対してregexがどのように使用されているか見てみましょう。

```TypeScript
// メールアドレスのregexパターンを定義する
const emailPattern = /\S+@\S+\.\S+/;

// 文字列がメールパターンにマッチするかテストする
const email = "user@example.com";
console.log(emailPattern.test(email)); // 出力: true

// 文字列の数字を見つけて置換する
const replaceDigits = "Item 25 costs $30".replace(/\d+/g, '#');
console.log(replaceDigits); // 出力: "Item # costs $#"

// 文字列から特定の部分を抽出するためにキャプチャグループを使用する
const data = "April 10, 2021";
const datePattern = /(\w+) (\d+), (\d+)/;
const [, month, day, year] = datePattern.exec(data) || [];
console.log(month, day, year); // 出力: "April" "10" "2021"
```

## 深掘り
1950年代に、数学者のスティーブン・クリーネは正規言語を表すモデルとして正規表現を記述しました。それは後にコンピュータサイエンスにおいて必須のものとなりました。時は流れ、現在ではテキストを扱うプログラミングにおいてregexは無くてはならないものです。

regexは文字列操作のためのスイスアーミーナイフですが、それに代わるものもあります。タスクの複雑さに応じて、`includes()`、`startsWith()`、`endsWith()`といった文字列メソッドや、ライブラリによる解析がより良いこともあります。例えば、複雑なJSON文字列をregexで解析するのは悪夢です。代わりにJSONパーサーを使いましょう。

実装に関しては、JavaScriptとTypeScriptのregexはECMAScript言語仕様に基づいています。内部では、エンジンはパターンを効率的にマッチさせるために状態機械を使用しています。regex操作は特に貧弱なパターンでの使用はパフォーマンスにおいてコストがかかることがあり、"災害的バックトラッキング"に注意することが重要です。

## 参照
- 正規表現についてのMDN Web Docs: [MDN Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- 正規表現のパターンのテストとデバッグのためのツールRegex101: [Regex101](https://regex101.com/)
- 深い理解のための"Mastering Regular Expressions"という本: [O'Reilly](https://www.oreilly.com/library/view/mastering-regular-expressions/0596528124/)
