---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:24.008893-07:00
description: "\u5904\u7406 CSV\uFF08\u9017\u53F7\u5206\u9694\u503C\uFF09\u6D89\u53CA\
  \u8BFB\u53D6\u548C\u5199\u5165 CSV \u6587\u4EF6\uFF0C\u8FD9\u662F\u4E00\u79CD\u5E38\
  \u89C1\u7684\u6570\u636E\u4EA4\u6362\u683C\u5F0F\uFF0C\u7531\u4E8E\u5176\u7B80\u5355\
  \u6027\u4EE5\u53CA\u5728\u5404\u79CD\u5E73\u53F0\u548C\u8BED\u8A00\u4E2D\u7684\u5E7F\
  \u6CDB\u652F\u6301\u800C\u88AB\u4F7F\u7528\u3002\u7A0B\u5E8F\u5458\u4E0E CSV \u6587\
  \u4EF6\u6253\u4EA4\u9053\u662F\u4E3A\u4E86\u4ECE\u5E94\u7528\u7A0B\u5E8F\u3001\u6570\
  \u636E\u5E93\u548C\u670D\u52A1\u4E2D\u5BFC\u5165\u6216\u5BFC\u51FA\u6570\u636E\uFF0C\
  \u4F7F\u5F97\u6570\u636E\u64CD\u4F5C\u548C\u5171\u4EAB\u53D8\u5F97\u5BB9\u6613\u3002"
lastmod: '2024-02-25T18:49:45.060751-07:00'
model: gpt-4-0125-preview
summary: "\u5904\u7406 CSV\uFF08\u9017\u53F7\u5206\u9694\u503C\uFF09\u6D89\u53CA\u8BFB\
  \u53D6\u548C\u5199\u5165 CSV \u6587\u4EF6\uFF0C\u8FD9\u662F\u4E00\u79CD\u5E38\u89C1\
  \u7684\u6570\u636E\u4EA4\u6362\u683C\u5F0F\uFF0C\u7531\u4E8E\u5176\u7B80\u5355\u6027\
  \u4EE5\u53CA\u5728\u5404\u79CD\u5E73\u53F0\u548C\u8BED\u8A00\u4E2D\u7684\u5E7F\u6CDB\
  \u652F\u6301\u800C\u88AB\u4F7F\u7528\u3002\u7A0B\u5E8F\u5458\u4E0E CSV \u6587\u4EF6\
  \u6253\u4EA4\u9053\u662F\u4E3A\u4E86\u4ECE\u5E94\u7528\u7A0B\u5E8F\u3001\u6570\u636E\
  \u5E93\u548C\u670D\u52A1\u4E2D\u5BFC\u5165\u6216\u5BFC\u51FA\u6570\u636E\uFF0C\u4F7F\
  \u5F97\u6570\u636E\u64CD\u4F5C\u548C\u5171\u4EAB\u53D8\u5F97\u5BB9\u6613\u3002"
title: "\u5904\u7406CSV\u6587\u4EF6"
---

{{< edit_this_page >}}

## 什么与为什么？

处理 CSV（逗号分隔值）涉及读取和写入 CSV 文件，这是一种常见的数据交换格式，由于其简单性以及在各种平台和语言中的广泛支持而被使用。程序员与 CSV 文件打交道是为了从应用程序、数据库和服务中导入或导出数据，使得数据操作和共享变得容易。

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
