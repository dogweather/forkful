---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:58.094207-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.696035-06:00'
model: gpt-4-0125-preview
summary: "JavaScript\u3067\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\u5B58\u5728\u3059\
  \u308B\u304B\u3092\u78BA\u8A8D\u3059\u308B\u3053\u3068\u306F\u3001\u30D5\u30A1\u30A4\
  \u30EB\u64CD\u4F5C\u30BF\u30B9\u30AF\u306B\u304A\u3044\u3066\u4E0D\u53EF\u6B20\u3067\
  \u3059\u3002\u3053\u308C\u306B\u3088\u308A\u3001\u30B9\u30AF\u30EA\u30D7\u30C8\u306F\
  \u8AAD\u307F\u53D6\u308A\u3084\u66F8\u304D\u8FBC\u307F\u306E\u524D\u306B\u30C7\u30A3\
  \u30EC\u30AF\u30C8\u30EA\u306E\u5B58\u5728\u3092\u691C\u8A3C\u3067\u304D\u307E\u3059\
  \u3002\u3053\u306E\u64CD\u4F5C\u306F\u30A8\u30E9\u30FC\u3092\u9632\u304E\u3001\u7279\
  \u306B\u30E6\u30FC\u30B6\u30FC\u5165\u529B\u3084\u5916\u90E8\u30C7\u30FC\u30BF\u30BD\
  \u30FC\u30B9\u306B\u57FA\u3065\u3044\u3066\u30D5\u30A1\u30A4\u30EB\u307E\u305F\u306F\
  \u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u3092\u52D5\u7684\u306B\u6271\u3046\u30A2\u30D7\
  \u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u306B\u304A\u3044\u3066\u3001\u30D7\u30ED\u30B0\
  \u30E9\u30E0\u306E\u5B9F\u884C\u3092\u3088\u308A\u30B9\u30E0\u30FC\u30BA\u306B\u3057\
  \u307E\u3059\u3002."
title: "\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\u5B58\u5728\u3059\u308B\u304B\u3069\
  \u3046\u304B\u306E\u78BA\u8A8D"
weight: 20
---

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
