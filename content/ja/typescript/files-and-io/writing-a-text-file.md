---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:56.051961-07:00
description: "\u65B9\u6CD5\uFF1A TypeScript\u81EA\u4F53\u306F\u30D5\u30A1\u30A4\u30EB\
  \u64CD\u4F5C\u3092\u76F4\u63A5\u6271\u308F\u306A\u3044\u305F\u3081\u3001JavaScript\u306B\
  \u30B3\u30F3\u30D1\u30A4\u30EB\u3055\u308C\u3001\u4F1D\u7D71\u7684\u306B\u306F\u30D5\
  \u30A1\u30A4\u30EB\u30B7\u30B9\u30C6\u30E0\u3078\u306E\u30A2\u30AF\u30BB\u30B9\u304C\
  \u9650\u3089\u308C\u305F\u30D6\u30E9\u30A6\u30B6\u3067\u5B9F\u884C\u3055\u308C\u307E\
  \u3059\u3002\u3057\u304B\u3057\u3001Node.js\u74B0\u5883\u3067\u4F7F\u7528\u3055\u308C\
  \u308B\u5834\u5408\u3001`fs`\u30E2\u30B8\u30E5\u30FC\u30EB\uFF08\u30D5\u30A1\u30A4\
  \u30EB\u30B7\u30B9\u30C6\u30E0\uFF09\u304C\u30D5\u30A1\u30A4\u30EB\u306E\u66F8\u304D\
  \u8FBC\u307F\u6A5F\u80FD\u3092\u63D0\u4F9B\u3057\u307E\u3059\u3002"
lastmod: '2024-04-05T22:37:50.078364-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A TypeScript\u81EA\u4F53\u306F\u30D5\u30A1\u30A4\u30EB\u64CD\
  \u4F5C\u3092\u76F4\u63A5\u6271\u308F\u306A\u3044\u305F\u3081\u3001JavaScript\u306B\
  \u30B3\u30F3\u30D1\u30A4\u30EB\u3055\u308C\u3001\u4F1D\u7D71\u7684\u306B\u306F\u30D5\
  \u30A1\u30A4\u30EB\u30B7\u30B9\u30C6\u30E0\u3078\u306E\u30A2\u30AF\u30BB\u30B9\u304C\
  \u9650\u3089\u308C\u305F\u30D6\u30E9\u30A6\u30B6\u3067\u5B9F\u884C\u3055\u308C\u307E\
  \u3059\u3002\u3057\u304B\u3057\u3001Node.js\u74B0\u5883\u3067\u4F7F\u7528\u3055\u308C\
  \u308B\u5834\u5408\u3001`fs`\u30E2\u30B8\u30E5\u30FC\u30EB\uFF08\u30D5\u30A1\u30A4\
  \u30EB\u30B7\u30B9\u30C6\u30E0\uFF09\u304C\u30D5\u30A1\u30A4\u30EB\u306E\u66F8\u304D\
  \u8FBC\u307F\u6A5F\u80FD\u3092\u63D0\u4F9B\u3057\u307E\u3059\u3002"
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210"
weight: 24
---

## 方法：
TypeScript自体はファイル操作を直接扱わないため、JavaScriptにコンパイルされ、伝統的にはファイルシステムへのアクセスが限られたブラウザで実行されます。しかし、Node.js環境で使用される場合、`fs`モジュール（ファイルシステム）がファイルの書き込み機能を提供します。

### Node.js fsモジュールの使用
まず、Node.js環境で作業していることを確認してください。それから、`fs`モジュールを使用してテキストファイルを書き込みます。基本的な例をこちらに示します：

```typescript
import * as fs from 'fs';

const data = 'こんにちは、世界！';
const filePath = './message.txt';

fs.writeFile(filePath, data, 'utf8', (err) => {
    if (err) throw err;
    console.log('ファイルが保存されました！');
});
```

これにより、「こんにちは、世界！」が`message.txt`に非同期で書き込まれます。ファイルが存在しない場合、Node.jsはそれを作成します。すでに存在する場合は、Node.jsがそれを上書きします。

同期的なファイル書き込みには、`writeFileSync`を使用します：

```typescript
import * as fs from 'fs';

const data = '再びこんにちは、世界！';
const filePath = './message.txt';

try {
    fs.writeFileSync(filePath, data, 'utf8');
    console.log('ファイルが保存されました！');
} catch (err) {
    console.error(err);
}
```

### 人気のあるサードパーティライブラリの使用
ネイティブの`fs`モジュールが強力であるにもかかわらず、一部の開発者は追加の便利さと機能性のためにサードパーティのライブラリを使用することを好みます。`fs-extra`は、`fs`を拡張し、ファイル操作をより簡単にする人気の選択肢です。

まず、`fs-extra`をインストールする必要があります：

```
npm install fs-extra
```

それから、TypeScriptファイルでテキストコンテンツを書くためにそれを使用できます：

```typescript
import * as fs from 'fs-extra';

const data = 'これはfs-extraです！';
const filePath = './extraMessage.txt';

// async/awaitを使用
async function writeFile() {
    try {
        await fs.writeFile(filePath, data, 'utf8');
        console.log('fs-extraでファイルが保存されました！');
    } catch (err) {
        console.error(err);
    }
}

writeFile();
```

このコードスニペットは、以前の`fs`の例と同じことを行いますが、プロミスの扱いをよりクリーンな構文で提供する`fs-extra`ライブラリを利用しています。
