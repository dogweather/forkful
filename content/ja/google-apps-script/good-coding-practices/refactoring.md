---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:00:08.576651-07:00
description: "\u65B9\u6CD5: Google Apps Script\u306B\u304A\u3044\u3066\u3001\u30EA\
  \u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0\u306E\u6069\u6075\u3092\u53D7\u3051\u308B\
  \u4E00\u822C\u7684\u306A\u30B7\u30CA\u30EA\u30AA\u306F\u3001Google\u2026"
lastmod: '2024-04-05T22:37:49.790748-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script\u306B\u304A\u3044\u3066\u3001\u30EA\u30D5\u30A1\u30AF\
  \u30BF\u30EA\u30F3\u30B0\u306E\u6069\u6075\u3092\u53D7\u3051\u308B\u4E00\u822C\u7684\
  \u306A\u30B7\u30CA\u30EA\u30AA\u306F\u3001Google Sheets\u3084Docs\u3068\u3084\u308A\
  \u53D6\u308A\u3059\u308B\u624B\u9593\u306E\u304B\u304B\u308B\u30B9\u30AF\u30EA\u30D7\
  \u30C8\u306E\u7C21\u7D20\u5316\u3067\u3059\u3002\u521D\u671F\u306B\u306F\u3001\u7D50\
  \u679C\u3092\u65E9\u304F\u5F97\u308B\u305F\u3081\u306B\u3001\u30B9\u30AF\u30EA\u30D7\
  \u30C8\u304C\u624B\u3063\u53D6\u308A\u65E9\u304F\u66F8\u304B\u308C\u308B\u3053\u3068\
  \u304C\u3042\u308A\u307E\u3059\u3002\u6642\u9593\u304C\u7D4C\u3064\u306B\u3064\u308C\
  \u3001\u30B9\u30AF\u30EA\u30D7\u30C8\u304C\u6210\u9577\u3059\u308B\u3068\u3001\u6271\
  \u3044\u306B\u304F\u304F\u306A\u308A\u307E\u3059\u3002\u3088\u308A\u8AAD\u307F\u3084\
  \u3059\u304F\u3001\u52B9\u7387\u7684\u306A\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\
  \u30B0\u306E\u4F8B\u3092\u7D39\u4ECB\u3057\u307E\u3057\u3087\u3046\u3002"
title: "\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0"
weight: 19
---

## 方法:
Google Apps Scriptにおいて、リファクタリングの恩恵を受ける一般的なシナリオは、Google SheetsやDocsとやり取りする手間のかかるスクリプトの簡素化です。初期には、結果を早く得るために、スクリプトが手っ取り早く書かれることがあります。時間が経つにつれ、スクリプトが成長すると、扱いにくくなります。より読みやすく、効率的なリファクタリングの例を紹介しましょう。

**オリジナルスクリプト:**

```javascript
function logSheetNames() {
  var sheets = SpreadsheetApp.getActiveSpreadsheet().getSheets();
  for (var i = 0; i < sheets.length; i++) {
    Logger.log(sheets[i].getName());
  }
}
```

この関数は、Google スプレッドシートの各シートの名前をログに記録します。問題なく動作しますが、古いJavaScriptの慣習を使用しており、明確さに欠けます。

**リファクタリングされたスクリプト:**

```javascript
function logSheetNames() {
  const sheets = SpreadsheetApp.getActiveSpreadsheet().getSheets();
  sheets.forEach(sheet => Logger.log(sheet.getName()));
}
```

リファクタリングされたバージョンでは、変更しない変数には`const`を使用することで、意図を明確にしました。また、配列を反復処理するより現代的で簡潔なアプローチである`forEach`メソッドも使用しており、可読性が向上しています。

**サンプル出力（両スクリプト共通）:**

Loggerの出力は以下のようになります。Google Sheetsドキュメントに「Expenses」と「Revenue」という名前の2つのシートがあると仮定します：

```
[20-04-2023 10:00:00: INFO] Expenses
[20-04-2023 10:00:01: INFO] Revenue
```

リファクタリングされたスクリプトは、同じ結果を達成しつつ、よりクリーンで一目で理解しやすくなっています。

## 深掘り
Google Apps Scriptでのリファクタリングは、その原則の一部を、より広いソフトウェアエンジニアリングの実践から継承しています。特に、1999年のマーティン・ファウラーの画期的な書籍「Refactoring: Improving the Design of Existing Code」によって、リファクタリングはさまざまな技法に関する包括的なガイドを提供し、概念としてより認識され、構造化されました。リファクタリングの具体的な内容はプログラミング言語の文法的および機能的な違いにより異なる場合がありますが、コアの目標は同じです：外部の動作を変更せずにコードを改善することです。

Google Apps Scriptの文脈では、リファクタリング中に考慮すべき主要な側面は、Googleによって課されるサービスのクオータと制限です。効率的にリファクタリングされたコードは、読みやすくだけでなく、これらの制約の中でより速く、より信頼性高く実行されます。たとえば、バッチ操作（一度に一つのセルに値を設定する代わりに`Range.setValues()`を使用するなど）は、実行時間とクオータの消費を大幅に削減することができます。

ただし、特定の複雑なプロジェクトの場合、これらの制限のためにGoogle Apps Scriptが不足することがあります。そのような場合、Google Cloud FunctionsやApps Scriptの新しい兄弟であるAppSheetを検討すると、より良いスケーラビリティと機能性を提供するかもしれません。

結局のところ、Google Apps Scriptプロジェクトを維持し、改善する上でリファクタリングは重要なスキルですが、環境の制限を理解し、代替解決策を検討することも、効率的で堅牢で、メンテナンス可能なコードを提供する上で同じくらい重要です。
