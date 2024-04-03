---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:55:32.031295-07:00
description: "Google Apps Script\u3067\u306E\u6587\u5B57\u5217\u5185\u633F\u306F\u3001\
  \u5F0F\u3092\u6587\u5B57\u5217\u5185\u306B\u52D5\u7684\u306B\u57CB\u3081\u8FBC\u3080\
  \u3053\u3068\u3092\u53EF\u80FD\u306B\u3057\u3001\u3088\u308A\u8AAD\u307F\u3084\u3059\
  \u304F\u3001\u4FDD\u5B88\u3057\u3084\u3059\u3044\u30B3\u30FC\u30C9\u306E\u4F5C\u6210\
  \u3092\u5BB9\u6613\u306B\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306F\u3053\u306E\u6280\u8853\u3092\u4F7F\u7528\u3057\u3066\u3001\u9762\u5012\u306A\
  \u9023\u7D50\u69CB\u6587\u306A\u3057\u3067\u5909\u6570\u3084\u5F0F\u3092\u6587\u5B57\
  \u5217\u306B\u30B7\u30FC\u30E0\u30EC\u30B9\u306B\u7D44\u307F\u8FBC\u3080\u3053\u3068\
  \u304C\u3067\u304D\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.426307-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script\u3067\u306E\u6587\u5B57\u5217\u5185\u633F\u306F\u3001\
  \u5F0F\u3092\u6587\u5B57\u5217\u5185\u306B\u52D5\u7684\u306B\u57CB\u3081\u8FBC\u3080\
  \u3053\u3068\u3092\u53EF\u80FD\u306B\u3057\u3001\u3088\u308A\u8AAD\u307F\u3084\u3059\
  \u304F\u3001\u4FDD\u5B88\u3057\u3084\u3059\u3044\u30B3\u30FC\u30C9\u306E\u4F5C\u6210\
  \u3092\u5BB9\u6613\u306B\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306F\u3053\u306E\u6280\u8853\u3092\u4F7F\u7528\u3057\u3066\u3001\u9762\u5012\u306A\
  \u9023\u7D50\u69CB\u6587\u306A\u3057\u3067\u5909\u6570\u3084\u5F0F\u3092\u6587\u5B57\
  \u5217\u306B\u30B7\u30FC\u30E0\u30EC\u30B9\u306B\u7D44\u307F\u8FBC\u3080\u3053\u3068\
  \u304C\u3067\u304D\u307E\u3059\u3002."
title: "\u6587\u5B57\u5217\u306E\u88DC\u9593"
weight: 8
---

## どのように：
Google Apps Scriptでの文字列内挿は、テンプレートリテラルを通じて達成されます。これは、組み込み式を許可する文字列リテラルで、通常の引用符の代わりにバックティック(`)で示されます。ここでは、それらを使用する方法です：

```javascript
// 基本的な例
function basicInterpolationExample() {
  const user = 'Alice';
  console.log(`こんにちは、${user}!`); // 出力：こんにちは、Alice!
}

// 式を使用する
function expressionInterpolationExample() {
  const a = 5;
  const b = 10;
  console.log(`五加十は${a + b}です。`); // 出力：五加十は15です。
}

// 複数行の文字列
function multiLineStringExample() {
  const item = 'Google Apps Script';
  console.log(`これは複数行の文字列です：
皆さん、こんにちは、
今日は${item}について話し合います。`);
  // 出力：
  // これは複数行の文字列です：
  // 皆さん、こんにちは、
  // 今日はGoogle Apps Scriptについて話し合います。
}

basicInterpolationExample();
expressionInterpolationExample();
multiLineStringExample();
```

これらの例は、基本的な使用方法、式の埋め込み、および内挿値を持つ複数行の文字列の作成を示しています。

## 深掘り
テンプレートリテラル（文字列内挿を含む）は、ECMAScript 2015（ES6）で導入され、その後Google Apps Scriptで採用されました。これ以前は、プログラマーは純粋に文字列の連結に頼らなければならず、複雑な文字列や多くの変数値を統合する場合には扱いにくくなりました。

```javascript
// ES6以前の古い方法
var user = 'Bob';
console.log('こんにちは、' + user + '！');
```

文字列内挿は強力な機能であるものの、正確な使用コンテキストに注意することが重要です。例えば、適切なサニタイズなしにユーザー入力を直接埋め込むことは、注入攻撃などのセキュリティ問題につながる可能性があります。Google Apps Script開発者は、文字列に内挿された動的コンテンツが適切にチェックまたはサニタイズされていることを確認する必要があります。

他のプログラミング言語と比較して、文字列内挿の概念は広く存在しますが、構文は異なります。Pythonではf-stringsまたは`format`メソッドを使用し、Rubyでは二重引用符内に`#{}`を使用し、多くの現代の言語が読みやすさと便利さのために類似の機能を採用しています。

Google Apps Scriptは、ECMAScript標準によって提供されるものを超える追加の内挿機能を提供していませんが、存在する機能はほとんどのユースケースにとって強力で十分です。より精巧な内挿機構を持つ言語から来た開発者は、期待を調整する必要があるかもしれませんが、Google Apps Scriptのテンプレートリテラルの単純さと効率性を高く評価することでしょう。
