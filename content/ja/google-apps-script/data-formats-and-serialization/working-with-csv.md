---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:05:35.004830-07:00
description: "Google Apps\u2026"
lastmod: '2024-03-13T22:44:41.475237-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script\u3067CSV\uFF08\u30AB\u30F3\u30DE\u533A\u5207\u308A\u5024\
  \uFF09\u30D5\u30A1\u30A4\u30EB\u3092\u6271\u3046\u3068\u3044\u3046\u3053\u3068\u306F\
  \u3001\u5404\u884C\u304C\u30C7\u30FC\u30BF\u30EC\u30B3\u30FC\u30C9\u3092\u8868\u3057\
  \u3001\u5024\u304C\u30AB\u30F3\u30DE\u3067\u533A\u5207\u3089\u308C\u305F\u30D7\u30EC\
  \u30FC\u30F3\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u3092\u8AAD\u307F\u53D6\
  \u308A\u3001\u4FEE\u6B63\u3057\u3001\u66F8\u304D\u8FBC\u3080\u3053\u3068\u3092\u610F\
  \u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001CSV\u304C\
  \u30B7\u30F3\u30D7\u30EB\u306A\u30C6\u30AD\u30B9\u30C8\u30D9\u30FC\u30B9\u306E\u30C7\
  \u30FC\u30BF\u4EA4\u63DB\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3068\u3057\u3066\u5E83\
  \u304F\u666E\u53CA\u3057\u3066\u3044\u308B\u305F\u3081\u3001\u7570\u306A\u308B\u30A2\
  \u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u3001\u30C7\u30FC\u30BF\u30D9\u30FC\u30B9\
  \u3001\u307E\u305F\u306F\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u8A00\u8A9E\u9593\
  \u3067\u30C7\u30FC\u30BF\u3092\u7C21\u5358\u306B\u4EA4\u63DB\u3059\u308B\u305F\u3081\
  \u306B\u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002."
title: "CSV\u3068\u306E\u4F5C\u696D"
weight: 37
---

## 何となぜ？

Google Apps ScriptでCSV（カンマ区切り値）ファイルを扱うということは、各行がデータレコードを表し、値がカンマで区切られたプレーンテキストファイルを読み取り、修正し、書き込むことを意味します。プログラマーは、CSVがシンプルなテキストベースのデータ交換フォーマットとして広く普及しているため、異なるアプリケーション、データベース、またはプログラミング言語間でデータを簡単に交換するためにこれを行います。

## 方法:

### CSVデータの読み取り

Google Driveに保存されているファイルからCSVデータを読み取るには、まずファイルの内容を文字列として取得し、それを解析する必要があります。Google Apps Scriptでは、DriveAppサービスを使用してファイル内容を取得するのが簡単です。

```javascript
function readCSV() {
  var fileId = 'YOUR_FILE_ID_HERE'; // 実際のファイルIDに置き換えてください
  var file = DriveApp.getFileById(fileId);
  var content = file.getBlob().getDataAsString();
  var rows = content.split("\n");
  
  for (var i = 0; i < rows.length; i++) {
    var cells = rows[i].split(",");
    Logger.log(cells); // 各行のセルをログに記録
  }
}
```

### CSVデータの書き込み

CSVファイルを作成して書き込むには、カンマ区切りの値と改行で構成される文字列を作成し、それを保存またはエクスポートします。この例では、Google Driveに新しいCSVファイルを作成する方法を示しています。

```javascript
function writeCSV() {
  var folderId = 'YOUR_FOLDER_ID_HERE'; // 新しいファイルを作成するDriveフォルダのIDに置き換えてください
  var csvContent = "Name,Age,Occupation\nJohn Doe,29,Engineer\nJane Smith,34,Designer";
  var fileName = "example.csv";
  
  var folder = DriveApp.getFolderById(folderId);
  folder.createFile(fileName, csvContent, MimeType.PLAIN_TEXT);
}
```

### サンプル出力

CSVから行セルを読み込んでログに記録するとき：

```plaintext
[John, 29, Engineer]
[Jane, 34, Designer]
```

書き込むと、"example.csv"という名前のファイルが以下の内容で作られます：

```plaintext
Name,Age,Occupation
John Doe,29,Engineer
Jane Smith,34,Designer
```

## より深く

歴史的に、CSVファイルはそのシンプルさと人間による可読性から、非プログラマーにもアクセスしやすく、迅速なデータ検査タスクに有用であると考えられてきました。しかし、Google Apps ScriptはGoogleのエコシステム内で動作し、Google SheetsはCSV操作のための強力でユーザーフレンドリーな代替手段として機能します。Sheetsはデータの編集にGUIを提供するだけでなく、複雑な数式やスタイリング、さらに多くの機能をサポートしており、生のCSVには欠けているものです。

Google Sheetsが提供する利点にもかかわらず、外部システムがCSV形式でデータを生成または要求する場合など、特に自動化されたタスクのために、Google Apps Scriptでの直接のCSV操作が重要です。たとえば、レガシーシステムとの統合、他のアプリケーションで使用するためのデータのエクスポート、またはGoogle Sheetsへのデータ供給前の前処理などです。

さらに、Google Apps ScriptはUtilitiesサービスを使用して高度なエンコーディングニーズを拡張したり、変換、解析、または検証タスクのために外部APIとインターフェースを合わせたりすることができます。しかし、大規模なデータセットを扱ったり、複雑な操作を必要とする場合には、より強力なデータ処理能力のためにGoogle Sheets APIを利用するか、BigQueryを探求することを検討してください。

シンプルさがCSVの人気の一因である一方で、これらの代替手段はGoogle Cloudエコシステム内でのデータ処理に富んだ機能セットを提供します。
