---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:49:08.306099-07:00
description: "\u65B9\u6CD5\uFF1A Google Apps Script\u306FJavaScript\u306B\u57FA\u3065\
  \u3044\u3066\u3044\u308B\u305F\u3081\u3001\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\
  \u306B\u3059\u308B\u305F\u3081\u306E\u3044\u304F\u3064\u304B\u306E\u65B9\u6CD5\u3092\
  \u63D0\u4F9B\u3057\u3066\u3044\u307E\u3059\u304C\u3001\u7D44\u307F\u8FBC\u307F\u95A2\
  \u6570\u306F\u3042\u308A\u307E\u305B\u3093\u3002\u3053\u3053\u306B\u7C21\u6F54\u306A\
  \u4F8B\u3092\u3044\u304F\u3064\u304B\u7D39\u4ECB\u3057\u307E\u3059\uFF1A **\u65B9\
  \u6CD5 1: charAt() \u3068 slice() \u3092\u4F7F\u7528\u3059\u308B**."
lastmod: '2024-04-05T21:53:42.367629-06:00'
model: gpt-4-0125-preview
summary: "charAt() \u3068 slice() \u3092\u4F7F\u7528\u3059\u308B**."
title: "\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u306B\u3059\u308B"
weight: 2
---

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
