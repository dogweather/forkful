---
aliases:
- /ja/google-apps-script/rounding-numbers/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:01:01.365728-07:00
description: "\u6570\u5024\u306E\u4E38\u3081\u3001\u30B3\u30F3\u30D4\u30E5\u30FC\u30BF\
  \u30FC\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u306E\u57FA\u672C\u7684\u306A\u6982\
  \u5FF5\u306F\u3001\u6570\u5024\u3092\u6700\u3082\u8FD1\u3044\u6574\u6570\u3084\u6307\
  \u5B9A\u3055\u308C\u305F\u5C0F\u6570\u70B9\u4EE5\u4E0B\u306E\u6841\u6570\u306B\u8ABF\
  \u6574\u3059\u308B\u3053\u3068\u3092\u542B\u307F\u307E\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u306F\u3001\u4EBA\u9593\u304C\u8AAD\u307F\u3084\u3059\u3044\u3088\
  \u3046\u306B\u6570\u5024\u3092\u5358\u7D14\u5316\u3057\u305F\u308A\u3001\u7279\u5B9A\
  \u306E\u8A08\u7B97\u30CB\u30FC\u30BA\u306B\u5408\u308F\u305B\u305F\u308A\u3059\u308B\
  \u305F\u3081\u306B\u4E38\u3081\u3092\u983B\u7E41\u306B\u884C\u3044\u307E\u3059\u3002\
  \u3053\u308C\u306B\u3088\u308A\u3001\u7CBE\u5BC6\u3055\u3092\u78BA\u4FDD\u3057\u3001\
  \u8A08\u7B97\u8CA0\u8377\u3092\u8EFD\u6E1B\u3057\u307E\u3059\u3002"
lastmod: 2024-02-18 23:08:54.517151
model: gpt-4-0125-preview
summary: "\u6570\u5024\u306E\u4E38\u3081\u3001\u30B3\u30F3\u30D4\u30E5\u30FC\u30BF\
  \u30FC\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u306E\u57FA\u672C\u7684\u306A\u6982\
  \u5FF5\u306F\u3001\u6570\u5024\u3092\u6700\u3082\u8FD1\u3044\u6574\u6570\u3084\u6307\
  \u5B9A\u3055\u308C\u305F\u5C0F\u6570\u70B9\u4EE5\u4E0B\u306E\u6841\u6570\u306B\u8ABF\
  \u6574\u3059\u308B\u3053\u3068\u3092\u542B\u307F\u307E\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u306F\u3001\u4EBA\u9593\u304C\u8AAD\u307F\u3084\u3059\u3044\u3088\
  \u3046\u306B\u6570\u5024\u3092\u5358\u7D14\u5316\u3057\u305F\u308A\u3001\u7279\u5B9A\
  \u306E\u8A08\u7B97\u30CB\u30FC\u30BA\u306B\u5408\u308F\u305B\u305F\u308A\u3059\u308B\
  \u305F\u3081\u306B\u4E38\u3081\u3092\u983B\u7E41\u306B\u884C\u3044\u307E\u3059\u3002\
  \u3053\u308C\u306B\u3088\u308A\u3001\u7CBE\u5BC6\u3055\u3092\u78BA\u4FDD\u3057\u3001\
  \u8A08\u7B97\u8CA0\u8377\u3092\u8EFD\u6E1B\u3057\u307E\u3059\u3002"
title: "\u6570\u5024\u306E\u56DB\u6368\u4E94\u5165"
---

{{< edit_this_page >}}

## 何となぜ？

数値の丸め、コンピュータープログラミングの基本的な概念は、数値を最も近い整数や指定された小数点以下の桁数に調整することを含みます。プログラマーは、人間が読みやすいように数値を単純化したり、特定の計算ニーズに合わせたりするために丸めを頻繁に行います。これにより、精密さを確保し、計算負荷を軽減します。

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
