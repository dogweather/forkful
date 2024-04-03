---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:01:01.365728-07:00
description: "\u65B9\u6CD5\uFF1A Google Apps Script\u306FJavaScript\u30D9\u30FC\u30B9\
  \u306E\u8A00\u8A9E\u3067\u3042\u308B\u305F\u3081\u3001\u6570\u5024\u3092\u4E38\u3081\
  \u308B\u305F\u3081\u306E\u6A19\u6E96\u7684\u306A\u65B9\u6CD5\u3092\u63D0\u4F9B\u3057\
  \u3066\u3044\u307E\u3059\u3002\u3053\u3053\u3067\u306F\u3001\u3088\u304F\u4F7F\u7528\
  \u3055\u308C\u308B3\u3064\u306E\u6280\u8853\u306E\u8A73\u7D30\u3092\u8AAC\u660E\u3057\
  \u307E\u3059\uFF1A #."
lastmod: '2024-03-13T22:44:41.437209-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script\u306FJavaScript\u30D9\u30FC\u30B9\u306E\u8A00\u8A9E\u3067\
  \u3042\u308B\u305F\u3081\u3001\u6570\u5024\u3092\u4E38\u3081\u308B\u305F\u3081\u306E\
  \u6A19\u6E96\u7684\u306A\u65B9\u6CD5\u3092\u63D0\u4F9B\u3057\u3066\u3044\u307E\u3059\
  \u3002\u3053\u3053\u3067\u306F\u3001\u3088\u304F\u4F7F\u7528\u3055\u308C\u308B3\u3064\
  \u306E\u6280\u8853\u306E\u8A73\u7D30\u3092\u8AAC\u660E\u3057\u307E\u3059\uFF1A\n\
  \n#."
title: "\u6570\u5024\u306E\u56DB\u6368\u4E94\u5165"
weight: 13
---

## 方法：
Google Apps ScriptはJavaScriptベースの言語であるため、数値を丸めるための標準的な方法を提供しています。ここでは、よく使用される3つの技術の詳細を説明します：

### Math.round()
この関数は、数値を最も近い整数に丸めます。

```javascript
var number = 2.56;
var roundedNumber = Math.round(number); 
Logger.log(roundedNumber); // 出力：3
```

### Math.ceil()
数値を最も近い整数に切り上げます。

```javascript
var number = 2.56;
var roundedUp = Math.ceil(number); 
Logger.log(roundedUp); // 出力：3
```

### Math.floor()
逆に、数値を最も近い整数に切り下げます。

```javascript
var number = 2.56;
var roundedDown = Math.floor(number); 
Logger.log(roundedDown); // 出力：2
```

特定の小数点以下の桁数を扱う場合、実際には文字列を返す`.toFixed()`を使うことができます。または、数学的な丸めに対してもっと洗練されたアプローチを使うことができます：

```javascript
var number = 2.56789;
var fixedNumber = number.toFixed(2); 
Logger.log(fixedNumber); // 出力："2.57"（文字列として）

var preciseRound = Math.round(number * 100) / 100; 
Logger.log(preciseRound); // 出力：2.57
```

## 深堀り
Google Apps Scriptでの数値の丸めは、他のJavaScript環境で行われるものとあまり変わりません。しかし、丸め方法の違いと、浮動小数点演算の問題の可能性を理解することは重要です。例えば、コンピュータが浮動小数点数を表現する方法のために、全ての十進分数を完璧な正確さで表現することはできず、時には予想外の丸め結果につながることがあります。

歴史的に、JavaScript（およびその拡張であるGoogle Apps Script）は、多くの他のプログラミング言語で浮動小数点演算に使用されているIEEE 754標準に準拠してこれを扱います。この標準は、数値がどのように丸められるかを定義し、さまざまなプラットフォームや言語間での一貫性を保証します。

Google Apps Scriptでの直接的な丸め方法は単純で十分な場合が多いですが、複雑な高精度のアプリケーションでは、任意の精度算術を扱うために設計されたライブラリ、例えばdecimal.jsやbig.jsが有益です。これらは、丸められた数値の精度が最優先される金融や科学の計算を行う場合に特に有用です。

ただし、Google Apps Scriptで外部ライブラリを活用するには、スクリプトエディタを通じてそれらを読み込む必要があり、使用方法によってはスクリプトのパフォーマンスに依存関係が生じたり影響を与えたりする可能性があります。多くのケースでは、組み込みのMathメソッドが完全に適切ですが、nth度の精度が必要なエッジケースでは、標準ライブラリを超えて見ることが必要になる場合があります。
