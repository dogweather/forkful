---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:53.378660-07:00
description: "TypeScript\u3067\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\u5B58\u5728\
  \u3059\u308B\u304B\u3092\u78BA\u8A8D\u3059\u308B\u3053\u3068\u306F\u3001\u30D5\u30A1\
  \u30A4\u30EB\u304B\u3089\u306E\u8AAD\u307F\u53D6\u308A\u3084\u30D5\u30A1\u30A4\u30EB\
  \u3078\u306E\u30C7\u30FC\u30BF\u66F8\u304D\u8FBC\u307F\u306A\u3069\u3001\u30D5\u30A1\
  \u30A4\u30EB\u7BA1\u7406\u30BF\u30B9\u30AF\u306B\u4E0D\u53EF\u6B20\u3067\u3059\u3002\
  \u3053\u308C\u306B\u3088\u308A\u3001\u64CD\u4F5C\u304C\u6709\u52B9\u306A\u30C7\u30A3\
  \u30EC\u30AF\u30C8\u30EA\u306B\u306E\u307F\u884C\u308F\u308C\u308B\u3053\u3068\u3092\
  \u4FDD\u8A3C\u3057\u3001\u5B58\u5728\u3057\u306A\u3044\u30C7\u30A3\u30EC\u30AF\u30C8\
  \u30EA\u306B\u30A2\u30AF\u30BB\u30B9\u3057\u305F\u308A\u64CD\u4F5C\u3057\u3088\u3046\
  \u3068\u3057\u305F\u969B\u306B\u751F\u3058\u308B\u30A8\u30E9\u30FC\u3092\u907F\u3051\
  \u308B\u3053\u3068\u304C\u91CD\u8981\u3067\u3059\u3002"
lastmod: '2024-02-25T18:49:39.839723-07:00'
model: gpt-4-0125-preview
summary: "TypeScript\u3067\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\u5B58\u5728\u3059\
  \u308B\u304B\u3092\u78BA\u8A8D\u3059\u308B\u3053\u3068\u306F\u3001\u30D5\u30A1\u30A4\
  \u30EB\u304B\u3089\u306E\u8AAD\u307F\u53D6\u308A\u3084\u30D5\u30A1\u30A4\u30EB\u3078\
  \u306E\u30C7\u30FC\u30BF\u66F8\u304D\u8FBC\u307F\u306A\u3069\u3001\u30D5\u30A1\u30A4\
  \u30EB\u7BA1\u7406\u30BF\u30B9\u30AF\u306B\u4E0D\u53EF\u6B20\u3067\u3059\u3002\u3053\
  \u308C\u306B\u3088\u308A\u3001\u64CD\u4F5C\u304C\u6709\u52B9\u306A\u30C7\u30A3\u30EC\
  \u30AF\u30C8\u30EA\u306B\u306E\u307F\u884C\u308F\u308C\u308B\u3053\u3068\u3092\u4FDD\
  \u8A3C\u3057\u3001\u5B58\u5728\u3057\u306A\u3044\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\
  \u306B\u30A2\u30AF\u30BB\u30B9\u3057\u305F\u308A\u64CD\u4F5C\u3057\u3088\u3046\u3068\
  \u3057\u305F\u969B\u306B\u751F\u3058\u308B\u30A8\u30E9\u30FC\u3092\u907F\u3051\u308B\
  \u3053\u3068\u304C\u91CD\u8981\u3067\u3059\u3002"
title: "\u30C7\u30A3\u30EC\u30AF\u30C8\u30EA\u304C\u5B58\u5728\u3059\u308B\u304B\u3069\
  \u3046\u304B\u306E\u78BA\u8A8D"
---

{{< edit_this_page >}}

## 何となぜ？
TypeScriptでディレクトリが存在するかを確認することは、ファイルからの読み取りやファイルへのデータ書き込みなど、ファイル管理タスクに不可欠です。これにより、操作が有効なディレクトリにのみ行われることを保証し、存在しないディレクトリにアクセスしたり操作しようとした際に生じるエラーを避けることが重要です。

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
