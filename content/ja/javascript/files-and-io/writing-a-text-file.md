---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:50.060752-07:00
description: "\u65B9\u6CD5\uFF1A Node.js\u74B0\u5883\u3067\u306F\u3001\u7D44\u307F\
  \u8FBC\u307F\u306E`fs`\uFF08\u30D5\u30A1\u30A4\u30EB\u30B7\u30B9\u30C6\u30E0\uFF09\
  \u30E2\u30B8\u30E5\u30FC\u30EB\u3092\u4F7F\u7528\u3057\u3066\u30C6\u30AD\u30B9\u30C8\
  \u30D5\u30A1\u30A4\u30EB\u306B\u66F8\u304D\u8FBC\u3080\u3053\u3068\u304C\u3067\u304D\
  \u307E\u3059\u3002\u3053\u306E\u4F8B\u306F\u3001\u975E\u540C\u671F\u306B\u30D5\u30A1\
  \u30A4\u30EB\u306B\u30C6\u30AD\u30B9\u30C8\u3092\u66F8\u304D\u8FBC\u3080\u65B9\u6CD5\
  \u3092\u793A\u3057\u3066\u3044\u307E\u3059\uFF1A."
lastmod: '2024-03-13T22:44:42.700011-06:00'
model: gpt-4-0125-preview
summary: "Node.js\u74B0\u5883\u3067\u306F\u3001\u7D44\u307F\u8FBC\u307F\u306E`fs`\uFF08\
  \u30D5\u30A1\u30A4\u30EB\u30B7\u30B9\u30C6\u30E0\uFF09\u30E2\u30B8\u30E5\u30FC\u30EB\
  \u3092\u4F7F\u7528\u3057\u3066\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306B\
  \u66F8\u304D\u8FBC\u3080\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\u3053\u306E\
  \u4F8B\u306F\u3001\u975E\u540C\u671F\u306B\u30D5\u30A1\u30A4\u30EB\u306B\u30C6\u30AD\
  \u30B9\u30C8\u3092\u66F8\u304D\u8FBC\u3080\u65B9\u6CD5\u3092\u793A\u3057\u3066\u3044\
  \u307E\u3059\uFF1A."
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210"
weight: 24
---

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
