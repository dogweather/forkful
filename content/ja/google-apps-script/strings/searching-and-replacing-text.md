---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:01:19.029827-07:00
description: "Google Apps Script\u3067\u306E\u30C6\u30AD\u30B9\u30C8\u691C\u7D22\u3068\
  \u7F6E\u63DB\u306F\u3001\u30C9\u30AD\u30E5\u30E1\u30F3\u30C8\u3001\u30B9\u30D7\u30EC\
  \u30C3\u30C9\u30B7\u30FC\u30C8\u3001\u307E\u305F\u306F\u305D\u306E\u4ED6\u306E\u30BF\
  \u30A4\u30D7\u306EGoogle\u2026"
lastmod: '2024-03-13T22:44:41.424860-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script\u3067\u306E\u30C6\u30AD\u30B9\u30C8\u691C\u7D22\u3068\
  \u7F6E\u63DB\u306F\u3001\u30C9\u30AD\u30E5\u30E1\u30F3\u30C8\u3001\u30B9\u30D7\u30EC\
  \u30C3\u30C9\u30B7\u30FC\u30C8\u3001\u307E\u305F\u306F\u305D\u306E\u4ED6\u306E\u30BF\
  \u30A4\u30D7\u306EGoogle Apps\u30B3\u30F3\u30C6\u30F3\u30C4\u3067\u7279\u5B9A\u306E\
  \u6587\u5B57\u5217\u3092\u30D7\u30ED\u30B0\u30E9\u30E0\u3067\u7279\u5B9A\u3057\u3001\
  \u305D\u308C\u3089\u3092\u4ED6\u306E\u30C6\u30AD\u30B9\u30C8\u5024\u306B\u7F6E\u304D\
  \u63DB\u3048\u308B\u3053\u3068\u3092\u542B\u307F\u307E\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u306F\u3001\u5927\u91CF\u306E\u30B3\u30F3\u30C6\u30F3\u30C4\u306E\
  \u7DE8\u96C6\u3092\u81EA\u52D5\u5316\u3057\u305F\u308A\u3001\u4E00\u822C\u7684\u306A\
  \u30A8\u30E9\u30FC\u3092\u4FEE\u6B63\u3057\u305F\u308A\u3001\u30C9\u30AD\u30E5\u30E1\
  \u30F3\u30C8\u9593\u3067\u7528\u8A9E\u3092\u6A19\u6E96\u5316\u3057\u305F\u308A\u3001\
  \u30C6\u30F3\u30D7\u30EC\u30FC\u30C8\u306B\u52D5\u7684\u30C7\u30FC\u30BF\u3092\u633F\
  \u5165\u3057\u305F\u308A\u3059\u308B\u305F\u3081\u306B\u3001\u3053\u306E\u6A5F\u80FD\
  \u3092\u5229\u7528\u3057\u307E\u3059\u3002."
title: "\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB"
weight: 10
---

## 方法：
Google Apps Scriptは、特にGoogle DocsとSheets内で、テキストを検索して置換する簡単な方法を提供します。以下に、両方の例を示します。

### Google Docs：
Googleドキュメント内のテキストを検索して置換するには、主に`DocumentApp`クラスと対話します。

```javascript
function searchReplaceInDoc() {
  var doc = DocumentApp.getActiveDocument();
  var body = doc.getBody();
  
  // 特定のフレーズを検索して置換する
  body.replaceText('searchText', 'replacementText');
  
  DocumentApp.getActiveDocument().saveAndClose();
}

// 使用例
searchReplaceInDoc();
```

このコードスニペットは、アクティブなGoogleドキュメント内の`'searchText'`のすべての出現を`'replacementText'`に置き換えます。

### Google Sheets：
同様に、Google Sheetsでは、`SpreadsheetApp`を使用して検索および置換操作を実行できます：

```javascript
function searchReplaceInSheet() {
  var sheet = SpreadsheetApp.getActiveSpreadsheet().getActiveSheet();
  
  // 現在アクティブなシートで検索して置換
  // replaceText(searchText, replacement)
  sheet.createTextFinder('searchText').replaceAllWith('replacementText');
}

// 使用例
searchReplaceInSheet();
```

この例では、`createTextFinder('searchText')`はアクティブなシートで'searchText'を検索し、`replaceAllWith('replacementText')`はすべての出現を'replacementText'に置き換えます。

## 深堀り
Google Apps Scriptの検索および置換機能は、そのWebベースの性質に大きく影響を受けており、スクリプトがさまざまなGoogle Appsでテキストをシームレスに操作できるようにしています。歴史的に、この機能は、PerlやPythonなどの言語での正規表現や文字列関数によって確立された柔軟性とパワーという、プログラミングにおけるテキスト処理と操作のより広い文脈から生じています。

Google Apps Scriptの検索および置換機能は、直接的な置換には強力ですが、他の言語で見られる完全な正規表現機能を欠いています。例えば、Google Sheetsの`createTextFinder`で基本的な正規表現を使用できますが、PerlやPythonに比べて、複雑なパターンマッチングおよび操作のオプションは限られています。

より高度なテキスト処理ニーズのために、プログラマーはGoogle DocsやSheetsのコンテンツを外部で処理できる形式にエクスポートしたり、より強力な言語を使用して外部で処理したり、外部のAPIやサービスを呼び出してより洗練されたテキスト操作機能を提供するためにGoogle Apps Scriptを使用することがあります。

これらの制限にもかかわらず、Google Appsのエコシステム内での典型的な検索および置換タスクに対して、Google Apps ScriptはGoogleの生産性ツールのスイート内での自動化およびスクリプティングニーズに特化したシンプルで効率的で高度に統合可能なソリューションを提供します。
