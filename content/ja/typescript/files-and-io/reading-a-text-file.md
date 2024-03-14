---
date: 2024-01-20 17:55:45.653551-07:00
description: "\u30D5\u30A1\u30A4\u30EB\u3092\u8AAD\u3080\u3053\u3068\u306F\u3001\u30C7\
  \u30A3\u30B9\u30AF\u4E0A\u306E\u30C6\u30AD\u30B9\u30C8\u30C7\u30FC\u30BF\u3092\u30D7\
  \u30ED\u30B0\u30E9\u30E0\u306B\u53D6\u308A\u8FBC\u3080\u4F5C\u696D\u3067\u3059\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30C7\u30FC\u30BF\u51E6\u7406\u3001\u8A2D\
  \u5B9A\u306E\u8AAD\u307F\u8FBC\u307F\u3001\u5916\u90E8\u30C7\u30FC\u30BF\u30BD\u30FC\
  \u30B9\u3068\u306E\u9023\u643A\u7B49\u306E\u305F\u3081\u306B\u3053\u308C\u3092\u884C\
  \u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.783253-06:00'
model: gpt-4-1106-preview
summary: "\u30D5\u30A1\u30A4\u30EB\u3092\u8AAD\u3080\u3053\u3068\u306F\u3001\u30C7\
  \u30A3\u30B9\u30AF\u4E0A\u306E\u30C6\u30AD\u30B9\u30C8\u30C7\u30FC\u30BF\u3092\u30D7\
  \u30ED\u30B0\u30E9\u30E0\u306B\u53D6\u308A\u8FBC\u3080\u4F5C\u696D\u3067\u3059\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30C7\u30FC\u30BF\u51E6\u7406\u3001\u8A2D\
  \u5B9A\u306E\u8AAD\u307F\u8FBC\u307F\u3001\u5916\u90E8\u30C7\u30FC\u30BF\u30BD\u30FC\
  \u30B9\u3068\u306E\u9023\u643A\u7B49\u306E\u305F\u3081\u306B\u3053\u308C\u3092\u884C\
  \u3044\u307E\u3059\u3002"
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u8AAD\u307F\u8FBC\u307F"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)

ファイルを読むことは、ディスク上のテキストデータをプログラムに取り込む作業です。プログラマーはデータ処理、設定の読み込み、外部データソースとの連携等のためにこれを行います。

## How to: (やり方)

以下にTypeScriptでテキストファイルを読み込むサンプルコードを示します。

```TypeScript
import { readFileSync } from 'fs';

try {
  const data = readFileSync('example.txt', 'utf8');
  console.log(data);
} catch (err) {
  console.error('ファイル読み込みエラー:', err);
}
```

出力例:
```
こんにちは、ファイルの世界へ！
```

非同期処理を利用する例です。

```TypeScript
import { readFile } from 'fs';

readFile('example.txt', 'utf8', (err, data) => {
  if (err) {
    console.error('ファイル読み込みエラー:', err);
  } else {
    console.log(data);
  }
});
```

## Deep Dive (掘り下げ)

歴史的に、ファイル読み込みはプログラミングの基本的な要素であり、様々な方法で実装されてきました。Node.jsでは、`fs`モジュールを使った同期と非同期のAPI両方を提供しています。前述の`readFileSync`は同期的処理で、リードが完了するまで待機します。これは起動時の設定ファイルの読み込みなど、直ちに処理が必要なシーンで有用です。しかし、ブロッキングが起きるとアプリケーションのパフォーマンスに影響するため、大半の場合は非同期処理が望ましいです。

`readFile`は非同期版で、コールバック関数に処理完了時のデータかエラーを渡します。非同期I/OはNode.jsの特長であり、大きなファイルやネットワーク経由の読み込みに適しています。

代替手段としてストリームも利用できます。これは大きなデータを小分けにして処理するときに便利です。また、TypeScriptを利用する場合は型定義をしっかりと行うことで、読み込んだデータの安全性を高めることができます。

## See Also (関連リンク)

- Node.js File System module: https://nodejs.org/api/fs.html
- Understanding asynchronous programming in Node.js: https://nodejs.dev/learn/understanding-javascript-asynchronous-programming
- Working with file streams in Node.js: https://nodejs.org/api/stream.html
