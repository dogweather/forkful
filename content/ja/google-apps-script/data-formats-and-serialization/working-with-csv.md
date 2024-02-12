---
title:                "CSVとの作業"
aliases: - /ja/google-apps-script/working-with-csv.md
date:                  2024-02-01T22:05:35.004830-07:00
model:                 gpt-4-0125-preview
simple_title:         "CSVとの作業"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/google-apps-script/working-with-csv.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
