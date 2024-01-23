---
title:                "テキストファイルの書き込み"
html_title:           "Bash: テキストファイルの書き込み"
simple_title:         "テキストファイルの書き込み"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
テキストファイルの書き込みって？プログラマーが使う理由は？
JavaScriptでテキストファイルを書き込むのはデータの保存や共有のため。簡単に自動化できて、バックアップや設定の管理に役立つから。

## How to:
### テキストファイルへの書き込み (Node.jsを使用)
```Javascript
const fs = require('fs');

// 同期的にファイルに書き込む
fs.writeFileSync('example.txt', 'こんにちは、世界！');

// 非同期でファイルに書き込む
fs.writeFile('example.txt', 'こんにちは、世界！', (err) => {
  if (err) throw err;
  console.log('ファイルが保存されました！');
});
```
### 出力例
ファイルが保存されました！

## Deep Dive
### 歴史的コンテキスト
JavaScriptはウェブでの動作を主としていたが、Node.jsの登場でサーバーサイドでもファイル操作が可能に。昔はActiveXやJavaアプレットを使っていたことも。

### 代替手段
HTML5のFile APIやIndexedDBなど、ブラウザ上でのデータ持続化技術もある。サーバーへのAJAXリクエストを利用したデータの保存も一般的。

### 実装の詳細
Nodeの`fs`モジュールはローカルファイルシステムへの完全なアクセスを提供。`writeFileSync`はブロッキング、`writeFile`は非ブロッキングでの操作が可能。

## See Also
- Node.jsのファイルシステムドキュメント: https://nodejs.org/api/fs.html
- 浏览器File APIのガイド: https://developer.mozilla.org/en-US/docs/Web/API/File/Using_files_from_web_applications
- IndexedDBについてのMDNのドキュメント: https://developer.mozilla.org/en-US/docs/Web/API/IndexedDB_API
