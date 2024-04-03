---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:58.094207-07:00
description: "\u65B9\u6CD5: JavaScript\u81EA\u4F53\u306F\u30D5\u30A1\u30A4\u30EB\u30B7\
  \u30B9\u30C6\u30E0\u306B\u76F4\u63A5\u30A2\u30AF\u30BB\u30B9\u3059\u308B\u6A5F\u80FD\
  \u304C\u306A\u3044\u305F\u3081\u3001Node.js\u3067\u306F\u5178\u578B\u7684\u306B\
  `fs`\u30E2\u30B8\u30E5\u30FC\u30EB\u304C\u3053\u306E\u3088\u3046\u306A\u64CD\u4F5C\
  \u306B\u4F7F\u7528\u3055\u308C\u307E\u3059\u3002\u4EE5\u4E0B\u306F`fs.existsSync()`\u3092\
  \u4F7F\u7528\u3057\u3066\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\u5B58\u5728\u3059\
  \u308B\u304B\u3069\u3046\u304B\u3092\u78BA\u8A8D\u3059\u308B\u7C21\u5358\u306A\u65B9\
  \u6CD5\u3067\u3059\uFF1A."
lastmod: '2024-03-13T22:44:42.696035-06:00'
model: gpt-4-0125-preview
summary: "JavaScript\u81EA\u4F53\u306F\u30D5\u30A1\u30A4\u30EB\u30B7\u30B9\u30C6\u30E0\
  \u306B\u76F4\u63A5\u30A2\u30AF\u30BB\u30B9\u3059\u308B\u6A5F\u80FD\u304C\u306A\u3044\
  \u305F\u3081\u3001Node.js\u3067\u306F\u5178\u578B\u7684\u306B`fs`\u30E2\u30B8\u30E5\
  \u30FC\u30EB\u304C\u3053\u306E\u3088\u3046\u306A\u64CD\u4F5C\u306B\u4F7F\u7528\u3055\
  \u308C\u307E\u3059\u3002\u4EE5\u4E0B\u306F`fs.existsSync()`\u3092\u4F7F\u7528\u3057\
  \u3066\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\u5B58\u5728\u3059\u308B\u304B\u3069\
  \u3046\u304B\u3092\u78BA\u8A8D\u3059\u308B\u7C21\u5358\u306A\u65B9\u6CD5\u3067\u3059\
  \uFF1A."
title: "\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\u5B58\u5728\u3059\u308B\u304B\u3069\
  \u3046\u304B\u306E\u78BA\u8A8D"
weight: 20
---

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
