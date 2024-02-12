---
title:                "テキストファイルの作成"
aliases:
- /ja/javascript/writing-a-text-file/
date:                  2024-02-03T19:28:50.060752-07:00
model:                 gpt-4-0125-preview
simple_title:         "テキストファイルの作成"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/writing-a-text-file.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？
JavaScriptでテキストファイルを書くことは、ログ記録、ユーザー入力のエクスポート、または設定目的のために、データをシンプルで読みやすい形式で作成および保存することをしばしば指します。この機能は、アプリケーションプロセスの寿命を超えてデータを永続化する必要があるアプリケーションにとって重要であり、情報を保存し、後で取り出したり共有したりする方法を提供します。

## 方法：
Node.js環境では、組み込みの`fs`（ファイルシステム）モジュールを使用してテキストファイルに書き込むことができます。この例は、非同期にファイルにテキストを書き込む方法を示しています：

```javascript
const fs = require('fs');

const data = 'Hello, World! This is text to be written into a file.';

fs.writeFile('example.txt', data, (err) => {
  if (err) {
    throw err;
  }
  console.log('File has been written.');
});
```

サンプル出力：
```
File has been written.
```

同期的にファイルを書き込むには、`writeFileSync`を使用します：
```javascript
try {
  fs.writeFileSync('example.txt', data);
  console.log('File has been written.');
} catch (error) {
  console.error('Error writing file:', error);
}
```

モダンなウェブブラウザでは、ファイルシステムアクセスAPIがファイルの読み書きの機能を導入しています。しかし、その使用はユーザーの許可によって制限されます。ファイルを作成して書き込む方法は次のとおりです：

```javascript
if ('showSaveFilePicker' in window) {
  const handle = await window.showSaveFilePicker();
  const writable = await handle.createWritable();
  await writable.write('Hello, World! This is browser text file writing.');
  await writable.close();
}
```

より複雑なシナリオや大きなファイルを扱う場合には、ブラウザ用の`FileSaver.js`のようなサードパーティーライブラリを選ぶかもしれません：

```html
<script src="https://cdnjs.cloudflare.com/ajax/libs/FileSaver.js/2.0.2/FileSaver.min.js"></script>
<script>
  const blob = new Blob(["Hello, World! This is text from FileSaver.js."], {type: "text/plain;charset=utf-8"});
  saveAs(blob, "example.txt");
</script>
```

クライアントサイド（ブラウザ内）でのファイル書き込みはセキュリティ上の懸念から制限されており、ユーザーのローカルディスクへの保存が必要な操作は通常、その明示的な許可が必要であることを覚えておいてください。
