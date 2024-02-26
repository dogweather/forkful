---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:58.094207-07:00
description: "\u2026"
lastmod: '2024-02-25T18:49:40.640308-07:00'
model: gpt-4-0125-preview
summary: "\u2026"
title: "\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\u5B58\u5728\u3059\u308B\u304B\u3069\
  \u3046\u304B\u306E\u78BA\u8A8D"
---

{{< edit_this_page >}}

## 何のために？
JavaScriptでディレクトリが存在するかを確認することは、ファイル操作タスクにおいて不可欠です。これにより、スクリプトは読み取りや書き込みの前にディレクトリの存在を検証できます。この操作はエラーを防ぎ、特にユーザー入力や外部データソースに基づいてファイルまたはディレクトリを動的に扱うアプリケーションにおいて、プログラムの実行をよりスムーズにします。

## 方法:
JavaScript自体はファイルシステムに直接アクセスする機能がないため、Node.jsでは典型的に`fs`モジュールがこのような操作に使用されます。以下は`fs.existsSync()`を使用してディレクトリが存在するかどうかを確認する簡単な方法です：

```javascript
const fs = require('fs');

const directoryPath = './sample-directory';

// ディレクトリが存在するか確認
if (fs.existsSync(directoryPath)) {
  console.log('ディレクトリは存在します。');
} else {
  console.log('ディレクトリは存在しません。');
}
```
**サンプル出力:**
```
ディレクトリは存在します。
```
または、ブロッキングされない非同期アプローチには`fs.promises`と`async/await`を使用します：

```javascript
const fs = require('fs').promises;

async function checkDirectory(directoryPath) {
  try {
    await fs.access(directoryPath);
    console.log('ディレクトリは存在します。');
  } catch (error) {
    console.log('ディレクトリは存在しません。');
  }
}

checkDirectory('./sample-directory');
```
**サンプル出力:**
```
ディレクトリは存在します。
```

ファイルやディレクトリの操作を頻繁に使用するプロジェクトのために、ネイティブ`fs`モジュールの拡張であり、便利な追加メソッドを提供する`fs-extra`パッケージがあります。以下は`fs-extra`を使用して同じことを実現する方法です：

```javascript
const fs = require('fs-extra');

const directoryPath = './sample-directory';

// ディレクトリが存在するか確認
fs.pathExists(directoryPath)
  .then(exists => console.log(exists ? 'ディレクトリは存在します。' : 'ディレクトリは存在しません。'))
  .catch(err => console.error(err));
```
**サンプル出力:**
```
ディレクトリは存在します。
```

このアプローチにより、モダンなJavaScriptプラクティスとシームレスに統合される、クリーンで読みやすいコードを実現できます。
