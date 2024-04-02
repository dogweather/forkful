---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:49:08.306099-07:00
description: "\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u306B\u3059\u308B\u3068\u3044\
  \u3046\u306E\u306F\u3001\u5165\u529B\u3067\u6700\u521D\u306E\u6587\u5B57\u3092\u5927\
  \u6587\u5B57\u306B\u3057\u3001\u6B8B\u308A\u3092\u5C0F\u6587\u5B57\u306E\u307E\u307E\
  \u306B\u3059\u308B\u5909\u66F4\u3092\u542B\u3080\u3082\u306E\u3067\u3001\u540D\u524D\
  \u3084\u30BF\u30A4\u30C8\u30EB\u3092\u6574\u5F62\u3059\u308B\u305F\u3081\u306B\u3088\
  \u304F\u4F7F\u7528\u3055\u308C\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306F\u3001\u3053\u308C\u3092\u884C\u3046\u3053\u3068\u3067\u30C7\u30FC\u30BF\u306E\
  \u4E00\u8CAB\u6027\u3092\u4FDD\u8A3C\u3057\u3001\u30E6\u30FC\u30B6\u30FC\u30A4\u30F3\
  \u30BF\u30FC\u30D5\u30A7\u30FC\u30B9\u3084\u6587\u66F8\u5185\u306E\u53EF\u8AAD\u6027\
  \u3092\u5411\u4E0A\u3055\u305B\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.422152-06:00'
model: gpt-4-0125-preview
summary: "\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u306B\u3059\u308B\u3068\u3044\
  \u3046\u306E\u306F\u3001\u5165\u529B\u3067\u6700\u521D\u306E\u6587\u5B57\u3092\u5927\
  \u6587\u5B57\u306B\u3057\u3001\u6B8B\u308A\u3092\u5C0F\u6587\u5B57\u306E\u307E\u307E\
  \u306B\u3059\u308B\u5909\u66F4\u3092\u542B\u3080\u3082\u306E\u3067\u3001\u540D\u524D\
  \u3084\u30BF\u30A4\u30C8\u30EB\u3092\u6574\u5F62\u3059\u308B\u305F\u3081\u306B\u3088\
  \u304F\u4F7F\u7528\u3055\u308C\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306F\u3001\u3053\u308C\u3092\u884C\u3046\u3053\u3068\u3067\u30C7\u30FC\u30BF\u306E\
  \u4E00\u8CAB\u6027\u3092\u4FDD\u8A3C\u3057\u3001\u30E6\u30FC\u30B6\u30FC\u30A4\u30F3\
  \u30BF\u30FC\u30D5\u30A7\u30FC\u30B9\u3084\u6587\u66F8\u5185\u306E\u53EF\u8AAD\u6027\
  \u3092\u5411\u4E0A\u3055\u305B\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u306B\u3059\u308B"
weight: 2
---

## 何となぜ？

文字列を大文字にするというのは、入力で最初の文字を大文字にし、残りを小文字のままにする変更を含むもので、名前やタイトルを整形するためによく使用されます。プログラマーは、これを行うことでデータの一貫性を保証し、ユーザーインターフェースや文書内の可読性を向上させます。

## 方法：

Google Apps ScriptはJavaScriptに基づいているため、文字列を大文字にするためのいくつかの方法を提供していますが、組み込み関数はありません。ここに簡潔な例をいくつか紹介します：

**方法 1: charAt() と slice() を使用する**

```javascript
function capitalizeString(inputString) {
  if (!inputString) return '';
  return inputString.charAt(0).toUpperCase() + inputString.slice(1).toLowerCase();
}

// サンプル使用法
let result = capitalizeString('hello, world');
console.log(result);  // 出力: Hello, world
```

**方法 2: 正規表現を使用する**

エッジケースをよりエレガントに扱うために、正規表現ベースのソリューションを好む方向け：

```javascript
function capitalizeStringRegex(inputString) {
  return inputString.toLowerCase().replace(/^\w/, c => c.toUpperCase());
}

// サンプル使用法
let result = capitalizeStringRegex('hello, world');
console.log(result);  // 出力: Hello, world
```

両方の方法は、文字列の最初の文字を大文字にし、残りを小文字にすることを保証し、Google Sheetsの操作やApps Scriptを介したドキュメント編集を含むがこれに限らないさまざまなアプリケーションに適しています。

## 深堀り

Google Apps Scriptでの文字列を大文字にすることは、JavaScriptの強力な文字列操作機能を利用することで簡単です。歴史的に、`.capitalize()`のような組み込みメソッドを提供するPythonなどの言語があり、これによりJavaScriptおよびApps Scriptのプログラマーにやや追加の作業が発生します。しかし、JavaScript/Google Apps Scriptに組み込み関数がないことは、柔軟性を促進し、文字列操作技術の深い理解を促します。

複雑なシナリオでは、たとえば文字列内の各単語を大文字にする（タイトルケース）など、プログラマーは`split()`関数と`map()`関数を組み合わせて各単語を個別に処理することがあります。Google Apps Scriptは文字列の大文字化に直接的な方法を提供していませんが、既存のJavaScriptの文字列操作方法の使用は十分な柔軟性を提供し、開発者がそれぞれの特定のニーズに応じて文字列を効率的に扱うことを可能にします。

パフォーマンスと効率が最優先事項である場合、特に長い文字列や大規模なループ内の操作において、正規表現よりも直接の文字列操作の方がパフォーマンスが向上する可能性があることに注意する価値があります。しかし、Google Apps Script内のほとんどの実用的なアプリケーションでは、両方のアプローチが信頼できる解決策を提供します。
