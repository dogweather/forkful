---
date: 2024-01-20 17:54:47.684701-07:00
description: "How to: (\u65B9\u6CD5) JavaScript\u3067\u306F\u3001\u7279\u306BNode.js\u74B0\
  \u5883\u3067\u30D5\u30A1\u30A4\u30EB\u3092\u8AAD\u307F\u8FBC\u3080\u65B9\u6CD5\u304C\
  \u8907\u6570\u3042\u308A\u307E\u3059\u3002\u4EE5\u4E0B\u306F`fs`\u30E2\u30B8\u30E5\
  \u30FC\u30EB\u3092\u4F7F\u7528\u3057\u305F\u4F8B\u3067\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.699199-06:00'
model: gpt-4-1106-preview
summary: "JavaScript\u3067\u306F\u3001\u7279\u306BNode.js\u74B0\u5883\u3067\u30D5\u30A1\
  \u30A4\u30EB\u3092\u8AAD\u307F\u8FBC\u3080\u65B9\u6CD5\u304C\u8907\u6570\u3042\u308A\
  \u307E\u3059\u3002\u4EE5\u4E0B\u306F`fs`\u30E2\u30B8\u30E5\u30FC\u30EB\u3092\u4F7F\
  \u7528\u3057\u305F\u4F8B\u3067\u3059."
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u8AAD\u307F\u8FBC\u307F"
weight: 22
---

## How to: (方法)
JavaScriptでは、特にNode.js環境でファイルを読み込む方法が複数あります。以下は`fs`モジュールを使用した例です。

```javascript
const fs = require('fs');

// 同期的にファイルを読み込む
const data = fs.readFileSync('example.txt', { encoding: 'utf8', flag: 'r' });
console.log(data);

// 非同期的にファイルを読み込む
fs.readFile('example.txt', { encoding: 'utf8', flag: 'r' }, (err, data) => {
  if (err) {
    console.error(err);
    return;
  }
  console.log(data);
});
```

結果 (テキストファイルの内容による):

```
こんにちは、テキストファイルです！
```

## Deep Dive (詳細情報)
JavaScriptにおけるテキストファイルの読み込みは、主にサーバーサイドで行われる操作です。Node.jsが誕生する前は、JavaScriptはクライアントサイドのみで動作していました。しかしNode.jsの出現により、ファイルシステムへのアクセスが可能になりました。

その他に、ブラウザ上では`File API`を用いた読み込み方法があります。これはブラウザが直接ファイルを操作する際に使う技術です。

ファイルの読み込みメソッドは、`readFileSync`（同期的）と`readFile`（非同期的）の2種類があります。同期的処理は簡単ですが、大きなファイルを扱う場合は、アプリケーションのパフォーマンスに影響を与える可能性があります。一方、非同期処理はコールバック関数を介して結果を返し、これによりイベントドリブンなアプローチでコードを書くことができます。

## See Also (関連情報)
- Node.js `fs` documentation: [Node.js File System](https://nodejs.org/api/fs.html)
- MDN Web Docs on the File API: [Using files from web applications](https://developer.mozilla.org/en-US/docs/Web/API/File/Using_files_from_web_applications)
