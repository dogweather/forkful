---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:56.051961-07:00
description: "TypeScript\u3067\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u3092\
  \u66F8\u304F\u3053\u3068\u306F\u3001\u30C7\u30FC\u30BF\u6C38\u7D9A\u6027\u3001\u8A2D\
  \u5B9A\u3001\u3042\u308B\u3044\u306F\u30ED\u30B0\u751F\u6210\u306E\u305F\u3081\u306E\
  \u91CD\u8981\u306A\u30B9\u30AD\u30EB\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u3057\u3070\u3057\u3070\u3001\u30C7\u30FC\u30BF\u5206\u6790\u3001\u5831\
  \u544A\u3001\u307E\u305F\u306F\u5358\u7D14\u306B\u30BB\u30C3\u30B7\u30E7\u30F3\u9593\
  \u3067\u30E6\u30FC\u30B6\u30FC\u8A2D\u5B9A\u3092\u4FDD\u5B58\u3059\u308B\u3088\u3046\
  \u306A\u7406\u7531\u3067\u3001\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u306E\
  \u30E1\u30E2\u30EA\u5916\u3067\u30C7\u30FC\u30BF\u3092\u4FDD\u5B58\u30FB\u64CD\u4F5C\
  \u3059\u308B\u305F\u3081\u306B\u3053\u306E\u4F5C\u696D\u3092\u884C\u3044\u307E\u3059\
  \u3002"
lastmod: '2024-03-11T00:14:15.383037-06:00'
model: gpt-4-0125-preview
summary: "TypeScript\u3067\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u3092\u66F8\
  \u304F\u3053\u3068\u306F\u3001\u30C7\u30FC\u30BF\u6C38\u7D9A\u6027\u3001\u8A2D\u5B9A\
  \u3001\u3042\u308B\u3044\u306F\u30ED\u30B0\u751F\u6210\u306E\u305F\u3081\u306E\u91CD\
  \u8981\u306A\u30B9\u30AD\u30EB\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306F\u3057\u3070\u3057\u3070\u3001\u30C7\u30FC\u30BF\u5206\u6790\u3001\u5831\u544A\
  \u3001\u307E\u305F\u306F\u5358\u7D14\u306B\u30BB\u30C3\u30B7\u30E7\u30F3\u9593\u3067\
  \u30E6\u30FC\u30B6\u30FC\u8A2D\u5B9A\u3092\u4FDD\u5B58\u3059\u308B\u3088\u3046\u306A\
  \u7406\u7531\u3067\u3001\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u306E\u30E1\
  \u30E2\u30EA\u5916\u3067\u30C7\u30FC\u30BF\u3092\u4FDD\u5B58\u30FB\u64CD\u4F5C\u3059\
  \u308B\u305F\u3081\u306B\u3053\u306E\u4F5C\u696D\u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u4F5C\u6210"
---

{{< edit_this_page >}}

## 何となぜ？
TypeScriptでテキストファイルを書くことは、データ永続性、設定、あるいはログ生成のための重要なスキルです。プログラマーはしばしば、データ分析、報告、または単純にセッション間でユーザー設定を保存するような理由で、アプリケーションのメモリ外でデータを保存・操作するためにこの作業を行います。

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
