---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:47.570216-07:00
description: "\u65B9\u6CD5: TypeScript\u3067CSV\u30D5\u30A1\u30A4\u30EB\u3092\u6271\
  \u3046\u306B\u306F\u3001\u30CD\u30A4\u30C6\u30A3\u30D6\u30B3\u30FC\u30C9\u3092\u4F7F\
  \u7528\u3059\u308B\u304B\u3001`csv-parser`\uFF08\u8AAD\u307F\u53D6\u308A\u7528\uFF09\
  \u304A\u3088\u3073`csv-writer`\uFF08\u66F8\u304D\u8FBC\u307F\u7528\uFF09\u306E\u3088\
  \u3046\u306A\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u30E9\u30A4\u30D6\u30E9\u30EA\
  \u3092\u6D3B\u7528\u3067\u304D\u307E\u3059\u3002 \u307E\u305A\u3001npm\u7D4C\u7531\
  \u3067`csv-parser`\u3092\u30A4\u30F3\u30B9\u30C8\u30FC\u30EB\u3057\u307E\u3059\uFF1A\
  ."
lastmod: '2024-03-13T22:44:41.789879-06:00'
model: gpt-4-0125-preview
summary: "TypeScript\u3067CSV\u30D5\u30A1\u30A4\u30EB\u3092\u6271\u3046\u306B\u306F\
  \u3001\u30CD\u30A4\u30C6\u30A3\u30D6\u30B3\u30FC\u30C9\u3092\u4F7F\u7528\u3059\u308B\
  \u304B\u3001`csv-parser`\uFF08\u8AAD\u307F\u53D6\u308A\u7528\uFF09\u304A\u3088\u3073\
  `csv-writer`\uFF08\u66F8\u304D\u8FBC\u307F\u7528\uFF09\u306E\u3088\u3046\u306A\u30B5\
  \u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u6D3B\u7528\
  \u3067\u304D\u307E\u3059."
title: "CSV\u3068\u306E\u4F5C\u696D"
weight: 37
---

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
