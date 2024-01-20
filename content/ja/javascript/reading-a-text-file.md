---
title:                "テキストファイルの読み込み"
html_title:           "Bash: テキストファイルの読み込み"
simple_title:         "テキストファイルの読み込み"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 何となぜ？

テキストファイルの読み取りは、その名の通り、テキストファイルの内容を読み出す行為です。それはデータを処理し、様々な方法で利用するために、プログラマが行います。

## 方法:

Node.jsの標準モジュールである'fs'を使ってテキストファイルを読み取りましょう。

```Javascript
const fs = require('fs');

fs.readFile('example.txt', 'utf8', function(err, data) {
  if (err) {
    console.error('Error:', err);
    return;
  }
  console.log(data);
});
```

このコードは'example.txt'という名前のファイルを読みましょう。エラーがあればログに表示し、そうでなければそのデータを表示します。

## ディープダイブ

1. 歴史的文脈: テキストファイルの読み取りはコンピューティングの早い段階から存在しています。初期のコンピュータは主にテキストベースのファイルを操作していました。
2. 代替手段: 'fs'モジュールの代わりに、ストリームを使った読み取りも可能です。これは特に大きなファイルに適しています。
3. 実装の詳細:'fs'モジュールの'readFile'関数は非同期で、バックグラウンドでファイル読み取りを行います。これにより、他のコードの実行をブロックしないでファイルを読み込むことができます。

## 参考資料

1. Node.js 'fs'モジュールのドキュメンテーション: [https://nodejs.org/api/fs.html](https://nodejs.org/api/fs.html)
3. JavaScript非同期処理の解説: [https://developer.mozilla.org/ja/docs/Learn/JavaScript/Asynchronous](https://developer.mozilla.org/ja/docs/Learn/JavaScript/Asynchronous)