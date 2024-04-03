---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:33.610742-07:00
description: "\u6B63\u898F\u8868\u73FE\u3001\u307E\u305F\u306Fregex\u306F\u3001\u30D7\
  \u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u306B\u304A\u3051\u308B\u5F37\u529B\u306A\u30D1\
  \u30BF\u30FC\u30F3\u30DE\u30C3\u30C1\u30F3\u30B0\u53CA\u3073\u691C\u7D22\u30C4\u30FC\
  \u30EB\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30E6\u30FC\
  \u30B6\u30FC\u5165\u529B\u306E\u691C\u8A3C\u3001\u30C6\u30AD\u30B9\u30C8\u306E\u691C\
  \u7D22\u3001\u307E\u305F\u306F\u6587\u5B57\u5217\u306E\u64CD\u4F5C\u306A\u3069\u306E\
  \u30BF\u30B9\u30AF\u306Bregex\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002\u306A\u305C\
  \u306A\u3089\u3001\u305D\u308C\u306F\u52B9\u7387\u7684\u3067\u591A\u69D8\u6027\u306B\
  \u5BCC\u3093\u3067\u3044\u308B\u304B\u3089\u3067\u3059\u3002"
lastmod: '2024-03-13T22:44:41.742995-06:00'
model: gpt-4-0125-preview
summary: "\u6B63\u898F\u8868\u73FE\u3001\u307E\u305F\u306Fregex\u306F\u3001\u30D7\u30ED\
  \u30B0\u30E9\u30DF\u30F3\u30B0\u306B\u304A\u3051\u308B\u5F37\u529B\u306A\u30D1\u30BF\
  \u30FC\u30F3\u30DE\u30C3\u30C1\u30F3\u30B0\u53CA\u3073\u691C\u7D22\u30C4\u30FC\u30EB\
  \u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30E6\u30FC\u30B6\
  \u30FC\u5165\u529B\u306E\u691C\u8A3C\u3001\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\
  \u3001\u307E\u305F\u306F\u6587\u5B57\u5217\u306E\u64CD\u4F5C\u306A\u3069\u306E\u30BF\
  \u30B9\u30AF\u306Bregex\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002\u306A\u305C\u306A\
  \u3089\u3001\u305D\u308C\u306F\u52B9\u7387\u7684\u3067\u591A\u69D8\u6027\u306B\u5BCC\
  \u3093\u3067\u3044\u308B\u304B\u3089\u3067\u3059\u3002."
title: "\u6B63\u898F\u8868\u73FE\u306E\u4F7F\u7528"
weight: 11
---

## 何となぜ？
正規表現、またはregexは、プログラミングにおける強力なパターンマッチング及び検索ツールです。プログラマーは、ユーザー入力の検証、テキストの検索、または文字列の操作などのタスクにregexを使用します。なぜなら、それは効率的で多様性に富んでいるからです。

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
