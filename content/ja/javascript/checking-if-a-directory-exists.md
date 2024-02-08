---
title:                "ディレクトリが存在するかどうかの確認"
aliases:
- ja/javascript/checking-if-a-directory-exists.md
date:                  2024-02-03T19:07:58.094207-07:00
model:                 gpt-4-0125-preview
simple_title:         "ディレクトリが存在するかどうかの確認"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/javascript/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
