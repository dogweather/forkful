---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:47.570216-07:00
description: "\u2026"
lastmod: '2024-03-11T00:14:15.386984-06:00'
model: gpt-4-0125-preview
summary: "\u2026"
title: "CSV\u3068\u306E\u4F5C\u696D"
---

{{< edit_this_page >}}

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
