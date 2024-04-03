---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:57:36.324237-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:41.457545-06:00'
model: gpt-4-0125-preview
summary: "\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u89E3\u6790\u3059\u308B\
  \u3053\u3068\u306F\u3001\u65E5\u4ED8\u3092\u8868\u3059\u30C6\u30AD\u30B9\u30C8\u3092\
  \u65E5\u4ED8\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u306B\u5909\u63DB\u3057\u3001\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u304C\u6BD4\u8F03\u3001\u7B97\u8853\u3001\u30D5\u30A9\u30FC\
  \u30DE\u30C3\u30C8\u306A\u3069\u306E\u65E5\u4ED8\u95A2\u9023\u306E\u64CD\u4F5C\u3092\
  \u5B9F\u884C\u3067\u304D\u308B\u3088\u3046\u306B\u3059\u308B\u3053\u3068\u3092\u610F\
  \u5473\u3057\u307E\u3059\u3002\u3053\u308C\u306F\u3001\u30E6\u30FC\u30B6\u5165\u529B\
  \u306E\u51E6\u7406\u3001\u5916\u90E8\u30BD\u30FC\u30B9\u304B\u3089\u306E\u30C7\u30FC\
  \u30BF\u51E6\u7406\u3001\u3055\u307E\u3056\u307E\u306A\u5F62\u5F0F\u306E\u65E5\u4ED8\
  \u306E\u7BA1\u7406\u3001\u7279\u306B\u30B9\u30B1\u30B8\u30E5\u30FC\u30EA\u30F3\u30B0\
  \u3001\u30C7\u30FC\u30BF\u5206\u6790\u3001\u3042\u308B\u3044\u306F\u6642\u9593\u30D9\
  \u30FC\u30B9\u306E\u8A18\u9332\u3092\u542B\u3080\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\
  \u30E7\u30F3\u306B\u304A\u3044\u3066\u3001\u4E0D\u53EF\u6B20\u3067\u3059\u3002."
title: "\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u89E3\u6790\u3059\u308B"
weight: 30
---

## 何となぜ？

文字列から日付を解析することは、日付を表すテキストを日付オブジェクトに変換し、プログラマが比較、算術、フォーマットなどの日付関連の操作を実行できるようにすることを意味します。これは、ユーザ入力の処理、外部ソースからのデータ処理、さまざまな形式の日付の管理、特にスケジューリング、データ分析、あるいは時間ベースの記録を含むアプリケーションにおいて、不可欠です。

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
