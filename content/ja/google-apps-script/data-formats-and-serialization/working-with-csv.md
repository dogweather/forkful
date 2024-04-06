---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:05:35.004830-07:00
description: "\u65B9\u6CD5: Google Drive\u306B\u4FDD\u5B58\u3055\u308C\u3066\u3044\
  \u308B\u30D5\u30A1\u30A4\u30EB\u304B\u3089CSV\u30C7\u30FC\u30BF\u3092\u8AAD\u307F\
  \u53D6\u308B\u306B\u306F\u3001\u307E\u305A\u30D5\u30A1\u30A4\u30EB\u306E\u5185\u5BB9\
  \u3092\u6587\u5B57\u5217\u3068\u3057\u3066\u53D6\u5F97\u3057\u3001\u305D\u308C\u3092\
  \u89E3\u6790\u3059\u308B\u5FC5\u8981\u304C\u3042\u308A\u307E\u3059\u3002Google Apps\
  \ Script\u3067\u306F\u3001DriveApp\u30B5\u30FC\u30D3\u30B9\u3092\u4F7F\u7528\u3057\
  \u3066\u30D5\u30A1\u30A4\u30EB\u5185\u5BB9\u3092\u53D6\u5F97\u3059\u308B\u306E\u304C\
  \u7C21\u5358\u3067\u3059\u3002"
lastmod: '2024-04-05T21:53:42.429454-06:00'
model: gpt-4-0125-preview
summary: "Google Drive\u306B\u4FDD\u5B58\u3055\u308C\u3066\u3044\u308B\u30D5\u30A1\
  \u30A4\u30EB\u304B\u3089CSV\u30C7\u30FC\u30BF\u3092\u8AAD\u307F\u53D6\u308B\u306B\
  \u306F\u3001\u307E\u305A\u30D5\u30A1\u30A4\u30EB\u306E\u5185\u5BB9\u3092\u6587\u5B57\
  \u5217\u3068\u3057\u3066\u53D6\u5F97\u3057\u3001\u305D\u308C\u3092\u89E3\u6790\u3059\
  \u308B\u5FC5\u8981\u304C\u3042\u308A\u307E\u3059\u3002Google Apps Script\u3067\u306F\
  \u3001DriveApp\u30B5\u30FC\u30D3\u30B9\u3092\u4F7F\u7528\u3057\u3066\u30D5\u30A1\
  \u30A4\u30EB\u5185\u5BB9\u3092\u53D6\u5F97\u3059\u308B\u306E\u304C\u7C21\u5358\u3067\
  \u3059\u3002"
title: "CSV\u3068\u306E\u4F5C\u696D"
weight: 37
---

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
