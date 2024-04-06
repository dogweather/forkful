---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:53:14.174519-07:00
description: "\u65B9\u6CD5\uFF1A Google Apps Script\u3067\u6587\u5B57\u5217\u3092\u5C0F\
  \u6587\u5B57\u306B\u5909\u63DB\u3059\u308B\u3053\u3068\u306F\u3001\u30B9\u30AF\u30EA\
  \u30D7\u30C8\u74B0\u5883\u5185\u3067\u5229\u7528\u53EF\u80FD\u306A\u7D44\u307F\u8FBC\
  \u307FJavaScript\u30E1\u30BD\u30C3\u30C9\u306E\u304A\u304B\u3052\u3067\u7C21\u5358\
  \u3067\u3059\u3002\u4E3B\u306B\u4F7F\u3046\u30E1\u30BD\u30C3\u30C9\u306F`toLowerCase()`\u3067\
  \u3059\u3002\u3053\u3053\u306B\u305D\u306E\u5B9F\u88C5\u65B9\u6CD5\u3092\u793A\u3057\
  \u307E\u3059\uFF1A."
lastmod: '2024-04-05T22:37:49.757734-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A Google Apps Script\u3067\u6587\u5B57\u5217\u3092\u5C0F\
  \u6587\u5B57\u306B\u5909\u63DB\u3059\u308B\u3053\u3068\u306F\u3001\u30B9\u30AF\u30EA\
  \u30D7\u30C8\u74B0\u5883\u5185\u3067\u5229\u7528\u53EF\u80FD\u306A\u7D44\u307F\u8FBC\
  \u307FJavaScript\u30E1\u30BD\u30C3\u30C9\u306E\u304A\u304B\u3052\u3067\u7C21\u5358\
  \u3067\u3059\u3002\u4E3B\u306B\u4F7F\u3046\u30E1\u30BD\u30C3\u30C9\u306F`toLowerCase()`\u3067\
  \u3059\u3002\u3053\u3053\u306B\u305D\u306E\u5B9F\u88C5\u65B9\u6CD5\u3092\u793A\u3057\
  \u307E\u3059\uFF1A."
title: "\u6587\u5B57\u5217\u3092\u5C0F\u6587\u5B57\u306B\u5909\u63DB\u3059\u308B"
weight: 4
---

## 方法：
Google Apps Scriptで文字列を小文字に変換することは、スクリプト環境内で利用可能な組み込みJavaScriptメソッドのおかげで簡単です。主に使うメソッドは`toLowerCase()`です。ここにその実装方法を示します：

```javascript
function convertToLower() {
  var originalString = "Hello, WORLD!";
  var lowerCaseString = originalString.toLowerCase();
  
  Logger.log(lowerCaseString); // 出力：hello, world!
}
```

このシンプルな関数は、オリジナルの文字列を取り、`toLowerCase()`メソッドを適用し、結果を記録することを示しています。これは、大小文字を区別しない入力を扱う場合に特に便利です。たとえば、ユーザーがさまざまなケースで入力する可能性のあるメールアドレスを比較する場合などです。

さらに、配列データを扱う状況では、各要素を小文字に変換するために配列をマップすることができます：

```javascript
function convertArrayItemsToLower() {
  var namesArray = ["Alice", "BOB", "Charlie"];
  var lowerCaseNamesArray = namesArray.map(function(name) {
    return name.toLowerCase();
  });
  
  Logger.log(lowerCaseNamesArray); // 出力：[alice, bob, charlie]
}
```

この例は、データセット全体の一貫性を保証するために、複数の文字列データを扱う際の`toLowerCase()`の汎用性を強調しています。

## 深掘り
`toLowerCase()`メソッドは、JavaScriptから継承され、Google Apps Script内で利用されるもので、JavaScriptの初期バージョンから文字列操作の重要な部分となっています。その主な目的は、動的でユーザー対話型のWebアプリケーションの出現に伴って生じた、テキストデータの大文字小文字を区別しない処理を支援することです。そのシンプルさにもかかわらず、このメカニズムは大文字と小文字の複雑さを軽減することにより、データの検証、ソート、および検索アルゴリズムで重要な役割を果たします。

パフォーマンスの観点からは、変換プロセスは現代のJavaScriptエンジンで高度に最適化されていますが、不必要な処理オーバーヘッドを避けるために、大規模なデータ操作内でのその適用は慎重に行うべきです。

特に、複雑なパターンを扱う場合や、ロケール固有の変換が必要な場合に検討すべき代替手段は、`toLocaleLowerCase()`メソッドです。このバリアントは、文字を小文字に変換するためのロケール固有のルールを考慮しています。これは、複数の言語をサポートするアプリケーションにとって重要かもしれません：

```javascript
var stringWithUmlaut = "MÄRZ";
var lowerCaseUmlaut = stringWithUmlaut.toLocaleLowerCase('de-DE');

Logger.log(lowerCaseUmlaut); // 出力：märz
```

追加の複雑さにもかかわらず、`toLocaleLowerCase()`は国際的なアプリケーションにとって強力なツールであり、変換がユーザーのロケールの言語規範を尊重することを保証します。どちらの方法を選択しても、Google Apps Scriptでの文字列を小文字に変換することは、ユーザー入力と標準化されたデータ処理との間のギャップを橋渡しする、テキスト処理の本質的な部分として残ります。
