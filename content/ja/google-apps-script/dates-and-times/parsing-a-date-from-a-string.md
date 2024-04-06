---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:57:36.324237-07:00
description: "\u3069\u306E\u3088\u3046\u306B\u3057\u3066\uFF1A Google Apps Script\u3067\
  \u306F\u3001JavaScript\u306B\u57FA\u3065\u3044\u3066\u3044\u307E\u3059\u304C\u3001\
  \u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u89E3\u6790\u3059\u308B\u305F\u3081\
  \u306E\u3044\u304F\u3064\u304B\u306E\u30A2\u30D7\u30ED\u30FC\u30C1\u304C\u3042\u308A\
  \u307E\u3059\u3002\u4EE5\u4E0B\u306F\u3001\u30CD\u30A4\u30C6\u30A3\u30D6\u306EJavaScript\u30E1\
  \u30BD\u30C3\u30C9\u3068Google Apps Script\u306E\u30E6\u30FC\u30C6\u30A3\u30EA\u30C6\
  \u30A3\u3092\u4F7F\u7528\u3057\u305F\u4F8B\u3067\u3059\u3002 **`new\u2026"
lastmod: '2024-04-05T22:37:49.792592-06:00'
model: gpt-4-0125-preview
summary: "** Google Apps Script\u3067\u6587\u5B57\u5217\u3092\u65E5\u4ED8\u306B\u89E3\
  \u6790\u3059\u308B\u6700\u3082\u5358\u7D14\u306A\u65B9\u6CD5\u306F\u3001`Date`\u30AA\
  \u30D6\u30B8\u30A7\u30AF\u30C8\u306E\u30B3\u30F3\u30B9\u30C8\u30E9\u30AF\u30BF\u3092\
  \u4F7F\u7528\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u305F\u3060\u3057\u3001Date.parse()\
  \ \u30E1\u30BD\u30C3\u30C9\u306B\u3088\u3063\u3066\u8A8D\u8B58\u3055\u308C\u308B\
  \u5F62\u5F0F\u306E\u65E5\u4ED8\u6587\u5B57\u5217\u304C\u5FC5\u8981\u3067\u3059\uFF08\
  \u4F8B\u3048\u3070\u3001YYYY-MM-DD\uFF09\u3002"
title: "\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u89E3\u6790\u3059\u308B"
weight: 30
---

## どのようにして：
Google Apps Scriptでは、JavaScriptに基づいていますが、文字列から日付を解析するためのいくつかのアプローチがあります。以下は、ネイティブのJavaScriptメソッドとGoogle Apps Scriptのユーティリティを使用した例です。

**`new Date()` コンストラクタを使う:**

Google Apps Scriptで文字列を日付に解析する最も単純な方法は、`Date`オブジェクトのコンストラクタを使用することです。ただし、Date.parse() メソッドによって認識される形式の日付文字列が必要です（例えば、YYYY-MM-DD）。

```javascript
const dateString = '2023-04-01';
const dateObject = new Date(dateString);
Logger.log(dateObject); // Sat Apr 01 2023 00:00:00 GMT+0000 (UTC)をログに記録
```

**`Utilities.parseDate()` を使う:**

カスタム日付フォーマットに対してより柔軟性を持たせるために、Google Apps Scriptは `Utilities.parseDate()` を提供しています。このメソッドを使うと、日付フォーマット、タイムゾーン、およびロケールを指定できます。

```javascript
const dateString = '01-04-2023'; // DD-MM-YYYY
const format = 'dd-MM-yyyy';
const timezone = Session.getScriptTimeZone();
const dateObject = Utilities.parseDate(dateString, timezone, format);
Logger.log(dateObject); // スクリプトのタイムゾーンに応じて Sat Apr 01 2023 00:00:00 GMT+0000 (UTC)をログに記録
```

注: `Utilities.parseDate()` はより多くの制御を提供しますが、スクリプトのタイムゾーンに基づいてその挙動が変わる可能性があるので、アプリケーションが複数の地域で日付を扱う場合には、タイムゾーンを明示的に指定することが重要です。

## 深掘り
プログラミング言語での日付解析は、歴史的にさまざまな日付フォーマットやタイムゾーンの複雑さのために、課題に直面してきました。Google Apps Scriptのアプローチは、主にJavaScriptから派生しており、直接的な`Date`オブジェクトとより汎用性の高い`Utilities.parseDate()`関数の両方を提供することで、これを単純化しようとしています。しかしながら、各方法には制限があります；例えば、文字列を使用した`Date`コンストラクタに頼ると、日付フォーマットの解釈が異なるため、異なる環境での不一致が発生します。一方で、`Utilities.parseDate()`は、フォーマット、タイムゾーン、およびロケールの明確な理解が必要であり、特定のニーズに対してはやや複雑ですが、より信頼性が高いです。

Moment.jsのような代替のライブラリやサービス（新しいプロジェクトではLuxonを推奨）は、より豊富な機能やより良いゾーン処理を提供し、これらの課題に対処しています。ただし、外部ライブラリが制限されるGoogle Apps Scriptのコンテキストでは、組み込みのメソッドを効果的に理解して活用することが重要になります。他の言語から来たプログラマは、Google Apps Scriptでの日付処理のニュアンスを特有の挑戦と見なすかもしれませんが、利用可能なツールの深い理解とアプリケーションのグローバルな性質を慎重に考慮すれば、堅牢な日付解析を達成することができます。
