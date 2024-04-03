---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:47.570216-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:41.789879-06:00'
model: gpt-4-0125-preview
summary: "CSV\uFF08\u30AB\u30F3\u30DE\u533A\u5207\u308A\u5024\uFF09\u3092\u6271\u3046\
  \u3053\u3068\u306F\u3001CSV\u30D5\u30A1\u30A4\u30EB\u304B\u3089\u306E\u8AAD\u307F\
  \u53D6\u308A\u304A\u3088\u3073\u66F8\u304D\u8FBC\u307F\u3092\u542B\u307F\u307E\u3059\
  \u3002\u3053\u308C\u306F\u3001\u305D\u306E\u5358\u7D14\u3055\u3068\u3055\u307E\u3056\
  \u307E\u306A\u30D7\u30E9\u30C3\u30C8\u30D5\u30A9\u30FC\u30E0\u3084\u8A00\u8A9E\u3067\
  \u306E\u5E83\u7BC4\u306A\u30B5\u30DD\u30FC\u30C8\u306E\u305F\u3081\u306B\u4F7F\u7528\
  \u3055\u308C\u308B\u5171\u901A\u306E\u30C7\u30FC\u30BF\u4EA4\u63DB\u5F62\u5F0F\u3067\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30A2\u30D7\u30EA\u30B1\
  \u30FC\u30B7\u30E7\u30F3\u3001\u30C7\u30FC\u30BF\u30D9\u30FC\u30B9\u3001\u304A\u3088\
  \u3073\u30B5\u30FC\u30D3\u30B9\u304B\u3089\u306E\u30C7\u30FC\u30BF\u306E\u30A4\u30F3\
  \u30DD\u30FC\u30C8\u307E\u305F\u306F\u30A8\u30AF\u30B9\u30DD\u30FC\u30C8\u306E\u305F\
  \u3081\u306BCSV\u30D5\u30A1\u30A4\u30EB\u3092\u6271\u3044\u3001\u7C21\u5358\u306A\
  \u30C7\u30FC\u30BF\u64CD\u4F5C\u3068\u5171\u6709\u3092\u53EF\u80FD\u306B\u3057\u307E\
  \u3059\u3002."
title: "CSV\u3068\u306E\u4F5C\u696D"
weight: 37
---

## 何となぜ？

CSV（カンマ区切り値）を扱うことは、CSVファイルからの読み取りおよび書き込みを含みます。これは、その単純さとさまざまなプラットフォームや言語での広範なサポートのために使用される共通のデータ交換形式です。プログラマーは、アプリケーション、データベース、およびサービスからのデータのインポートまたはエクスポートのためにCSVファイルを扱い、簡単なデータ操作と共有を可能にします。

## 方法:

TypeScriptでCSVファイルを扱うには、ネイティブコードを使用するか、`csv-parser`（読み取り用）および`csv-writer`（書き込み用）のようなサードパーティライブラリを活用できます。

### `csv-parser`でのCSV読み取り

まず、npm経由で`csv-parser`をインストールします：

```
npm install csv-parser
```

次に、以下のようにCSVファイルを読み取ります：

```typescript
import fs from 'fs';
import csv from 'csv-parser';

const results = [];

fs.createReadStream('data.csv')
  .pipe(csv())
  .on('data', (data) => results.push(data))
  .on('end', () => {
    console.log(results);
    // 出力：CSVの各行を表すオブジェクトの配列
  });
```

`data.csv`が以下を含むと仮定します：

```
name,age
Alice,30
Bob,25
```

出力は以下になります：

```
[ { name: 'Alice', age: '30' }, { name: 'Bob', age: '25' } ]
```

### `csv-writer`でのCSV書き込み

CSVファイルに書き込むには、まず`csv-writer`をインストールします：

```
npm install csv-writer
```

次に、以下のように使用します：

```typescript
import { createObjectCsvWriter as createCsvWriter } from 'csv-writer';

const csvWriter = createCsvWriter({
  path: 'out.csv',
  header: [
    {id: 'name', title: 'NAME'},
    {id: 'age', title: 'AGE'}
  ]
});

const data = [
  { name: 'Alice', age: 30 },
  { name: 'Bob', age: 25 }
];

csvWriter
  .writeRecords(data)
  .then(() => console.log('CSVファイルの書き込みが成功しました'));
```

このコードは`out.csv`に以下を書き込みます：

```
NAME,AGE
Alice,30
Bob,25
```

これらの例は、データ分析のための読み取りやアプリケーションデータの外部への永続化など、TypeScriptプロジェクトでのCSV処理を効率的に統合する方法を示しています。
