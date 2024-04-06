---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:53.378660-07:00
description: "\u65B9\u6CD5\uFF1A TypeScript\u3092Node.js\u74B0\u5883\u3067\u5B9F\u884C\
  \u3059\u308B\u5834\u5408\u3001`fs`\u30E2\u30B8\u30E5\u30FC\u30EB\u3092\u4F7F\u7528\
  \u3057\u3066\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\u5B58\u5728\u3059\u308B\u304B\
  \u3092\u78BA\u8A8D\u3059\u308B\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\u3053\
  \u306E\u30E2\u30B8\u30E5\u30FC\u30EB\u306F\u3001`existsSync()`\u95A2\u6570\u3084\
  \u975E\u540C\u671F\u306E`access()`\u95A2\u6570\u3068`constants.F_OK`\u3092\u7D44\
  \u307F\u5408\u308F\u305B\u305F\u3082\u306E\u3092\u63D0\u4F9B\u3057\u307E\u3059\u3002"
lastmod: '2024-04-05T22:37:50.073230-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A TypeScript\u3092Node.js\u74B0\u5883\u3067\u5B9F\u884C\
  \u3059\u308B\u5834\u5408\u3001`fs`\u30E2\u30B8\u30E5\u30FC\u30EB\u3092\u4F7F\u7528\
  \u3057\u3066\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\u5B58\u5728\u3059\u308B\u304B\
  \u3092\u78BA\u8A8D\u3059\u308B\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\u3053\
  \u306E\u30E2\u30B8\u30E5\u30FC\u30EB\u306F\u3001`existsSync()`\u95A2\u6570\u3084\
  \u975E\u540C\u671F\u306E`access()`\u95A2\u6570\u3068`constants.F_OK`\u3092\u7D44\
  \u307F\u5408\u308F\u305B\u305F\u3082\u306E\u3092\u63D0\u4F9B\u3057\u307E\u3059\u3002"
title: "\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\u5B58\u5728\u3059\u308B\u304B\u3069\
  \u3046\u304B\u306E\u78BA\u8A8D"
weight: 20
---

## 方法：
TypeScriptをNode.js環境で実行する場合、`fs`モジュールを使用してディレクトリが存在するかを確認することができます。このモジュールは、`existsSync()`関数や非同期の`access()`関数と`constants.F_OK`を組み合わせたものを提供します。

### `fs.existsSync()`の使用：
```typescript
import { existsSync } from 'fs';

const directoryPath = './path/to/directory';

if (existsSync(directoryPath)) {
  console.log('ディレクトリが存在します。');
} else {
  console.log('ディレクトリが存在しません。');
}
```

### `fs.access()`と`fs.constants.F_OK`の使用：
```typescript
import { access, constants } from 'fs';

const directoryPath = './path/to/directory';

access(directoryPath, constants.F_OK, (err) => {
  if (err) {
    console.log('ディレクトリが存在しません。');
    return;
  }
  console.log('ディレクトリが存在します。');
});
```

**両方の方法を使った場合のサンプル出力**（ディレクトリが存在する場合）:
```
ディレクトリが存在します。
```

存在しない場合:
```
ディレクトリが存在しません。
```

### サードパーティのライブラリを使用する - `fs-extra`:
`fs-extra`は、組み込みの`fs`モジュールを強化し、より便利な機能を提供する人気のサードパーティライブラリです。

```typescript
import { pathExists } from 'fs-extra';

const directoryPath = './path/to/directory';

pathExists(directoryPath).then(exists => {
  console.log(`ディレクトリが存在します: ${exists}`);
});
```

**ディレクトリが存在する場合のサンプル出力**:
```
ディレクトリが存在します: true
```

存在しない場合:
```
ディレクトリが存在しません: false
```
