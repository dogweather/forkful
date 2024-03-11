---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:50:17.932776-07:00
description: "\u6587\u5B57\u5217\u306E\u9023\u7D50\u3068\u306F\u30012\u3064\u4EE5\u4E0A\
  \u306E\u6587\u5B57\u5217\u30921\u3064\u306E\u6587\u5B57\u5217\u306B\u7D44\u307F\u5408\
  \u308F\u305B\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u306F\
  \u3053\u308C\u3092\u884C\u3046\u3053\u3068\u3067\u3001\u52D5\u7684\u306B\u30E1\u30C3\
  \u30BB\u30FC\u30B8\u3001URL\u3001\u307E\u305F\u306F\u9759\u7684\u306A\u5185\u5BB9\
  \u3068\u5909\u6570\u306E\u5185\u5BB9\u306E\u6DF7\u5408\u304C\u5FC5\u8981\u306A\u3042\
  \u3089\u3086\u308B\u5F62\u614B\u306E\u30C6\u30AD\u30B9\u30C8\u3092\u69CB\u7BC9\u3057\
  \u307E\u3059\u3002"
lastmod: '2024-03-11T00:14:15.033357-06:00'
model: gpt-4-0125-preview
summary: "\u6587\u5B57\u5217\u306E\u9023\u7D50\u3068\u306F\u30012\u3064\u4EE5\u4E0A\
  \u306E\u6587\u5B57\u5217\u30921\u3064\u306E\u6587\u5B57\u5217\u306B\u7D44\u307F\u5408\
  \u308F\u305B\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u306F\
  \u3053\u308C\u3092\u884C\u3046\u3053\u3068\u3067\u3001\u52D5\u7684\u306B\u30E1\u30C3\
  \u30BB\u30FC\u30B8\u3001URL\u3001\u307E\u305F\u306F\u9759\u7684\u306A\u5185\u5BB9\
  \u3068\u5909\u6570\u306E\u5185\u5BB9\u306E\u6DF7\u5408\u304C\u5FC5\u8981\u306A\u3042\
  \u3089\u3086\u308B\u5F62\u614B\u306E\u30C6\u30AD\u30B9\u30C8\u3092\u69CB\u7BC9\u3057\
  \u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u9023\u7D50"
---

{{< edit_this_page >}}

## 何となぜ？

文字列の連結とは、2つ以上の文字列を1つの文字列に組み合わせることです。プログラマはこれを行うことで、動的にメッセージ、URL、または静的な内容と変数の内容の混合が必要なあらゆる形態のテキストを構築します。

## 方法

Google Apps Scriptでは、JavaScriptに基づいているため、文字列を連結する方法はいくつかあります。ここではいくつかの一般的な方法を紹介します：

### プラス演算子 (`+`) を使用する：

```javascript
var firstName = "John";
var lastName = "Doe";
var fullName = firstName + " " + lastName;
Logger.log(fullName); // 出力: John Doe
```

### `concat()` メソッドを使用する：

```javascript
var string1 = "Hello";
var string2 = "World";
var combinedString = string1.concat(" ", string2);
Logger.log(combinedString); // 出力: Hello World
```

### テンプレートリテラル（バックティック）を使用する：

これは文字列を連結するための現代的で柔軟な方法であり、文字列内に表現を簡単に埋め込むことができます。

```javascript
var language = "Google Apps Script";
var message = `Learning ${language} is fun!`;
Logger.log(message); // 出力: Learning Google Apps Script is fun!
```

これらの方法はそれぞれ使用シーンがあり、それらの選択は通常、文字列の可読性要件と複雑さに依存します。

## 深堀り

文字列の連結は、Google Apps Scriptだけでなく、多くのプログラミング言語の基本的な側面です。歴史的に、文字列の連結はしばしばプラス演算子や`concat()`のような特殊な関数/メソッドを使用して実行されました。しかし、ECMAScript 2015（ES6）でテンプレートリテラルが導入されたことにより、Google Apps Scriptがサポートしていることが、開発者により強力で直感的な文字列処理方法を提供しました。

テンプレートリテラルは、文字列内で表現を埋め込むための構文を単純化するだけでなく、明示的な改行文字なしで複数行の文字列をサポートします。これにより、エラーの可能性が減少し、特に複雑な文字列を扱う場合やテキストテンプレートに複数の変数を代入する場合に、コードの可読性が向上します。

`+`演算子と`concat()`メソッドは依然として後方互換性とシンプルなシナリオでの簡潔さの観点から広く使用されていますが、テンプレートリテラルは可読性と保守性が懸念される場合に特に優れていると考えられる現代的で表現豊かな代替手段を提供します。

それにもかかわらず、プロジェクトの特定のコンテキストと要件に最も適した方法を選択することが重要であり、Google Apps Scriptでの互換性の問題（これはめったに問題にならない）、パフォーマンスへの影響（ほとんどのアプリケーションにとって最小限）、および開発チームが現代のJavaScript機能に慣れているかどうかなどの要因を考慮してください。
