---
title:                "数値の四捨五入"
aliases: - /ja/google-apps-script/rounding-numbers.md
date:                  2024-02-01T22:01:01.365728-07:00
model:                 gpt-4-0125-preview
simple_title:         "数値の四捨五入"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/google-apps-script/rounding-numbers.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
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
