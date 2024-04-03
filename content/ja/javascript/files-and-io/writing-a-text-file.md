---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:50.060752-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.700011-06:00'
model: gpt-4-0125-preview
summary: "JavaScript\u3067\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u3092\u66F8\
  \u304F\u3053\u3068\u306F\u3001\u30ED\u30B0\u8A18\u9332\u3001\u30E6\u30FC\u30B6\u30FC\
  \u5165\u529B\u306E\u30A8\u30AF\u30B9\u30DD\u30FC\u30C8\u3001\u307E\u305F\u306F\u8A2D\
  \u5B9A\u76EE\u7684\u306E\u305F\u3081\u306B\u3001\u30C7\u30FC\u30BF\u3092\u30B7\u30F3\
  \u30D7\u30EB\u3067\u8AAD\u307F\u3084\u3059\u3044\u5F62\u5F0F\u3067\u4F5C\u6210\u304A\
  \u3088\u3073\u4FDD\u5B58\u3059\u308B\u3053\u3068\u3092\u3057\u3070\u3057\u3070\u6307\
  \u3057\u307E\u3059\u3002\u3053\u306E\u6A5F\u80FD\u306F\u3001\u30A2\u30D7\u30EA\u30B1\
  \u30FC\u30B7\u30E7\u30F3\u30D7\u30ED\u30BB\u30B9\u306E\u5BFF\u547D\u3092\u8D85\u3048\
  \u3066\u30C7\u30FC\u30BF\u3092\u6C38\u7D9A\u5316\u3059\u308B\u5FC5\u8981\u304C\u3042\
  \u308B\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u306B\u3068\u3063\u3066\u91CD\
  \u8981\u3067\u3042\u308A\u3001\u60C5\u5831\u3092\u4FDD\u5B58\u3057\u3001\u5F8C\u3067\
  \u53D6\u308A\u51FA\u3057\u305F\u308A\u5171\u6709\u3057\u305F\u308A\u3059\u308B\u65B9\
  \u6CD5\u3092\u63D0\u4F9B\u3057\u307E\u3059\u3002."
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210"
weight: 24
---

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
