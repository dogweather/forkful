---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:24.008893-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728 TypeScript \u4E2D\uFF0C\u60A8\u53EF\
  \u4EE5\u901A\u8FC7\u539F\u751F\u4EE3\u7801\u6216\u5229\u7528\u7B2C\u4E09\u65B9\u5E93\
  \u5982 `csv-parser` \u8FDB\u884C\u8BFB\u53D6\u548C `csv-writer` \u8FDB\u884C\u5199\
  \u5165 CSV \u6587\u4EF6\u3002"
lastmod: '2024-04-05T21:53:47.820806-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u5904\u7406CSV\u6587\u4EF6"
weight: 37
---

## 如何操作：
在 TypeScript 中，您可以通过原生代码或利用第三方库如 `csv-parser` 进行读取和 `csv-writer` 进行写入 CSV 文件。

### 使用 `csv-parser` 读取 CSV
首先，通过 npm 安装 `csv-parser`：

```
npm install csv-parser
```

然后，像这样读取一个 CSV 文件：

```typescript
import fs from 'fs';
import csv from 'csv-parser';

const results = [];

fs.createReadStream('data.csv')
  .pipe(csv())
  .on('data', (data) => results.push(data))
  .on('end', () => {
    console.log(results);
    // 输出：一个对象数组，每个对象代表 CSV 中的一行
  });
```

假设 `data.csv` 包含：

```
name,age
Alice,30
Bob,25
```

输出将会是：

```
[ { name: 'Alice', age: '30' }, { name: 'Bob', age: '25' } ]
```

### 使用 `csv-writer` 写入 CSV
要写入一个 CSV 文件，首先安装 `csv-writer`：

```
npm install csv-writer
```

然后，按照以下方式使用它：

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
  .then(() => console.log('The CSV file was written successfully'));
```

这段代码会写入如下内容到 `out.csv`：

```
NAME,AGE
Alice,30
Bob,25
```

这些示例展示了如何在您的 TypeScript 项目中有效地集成 CSV 处理，无论是为了分析数据还是持久化应用数据到外部。
