---
title:                "处理 CSV 文件"
date:                  2024-01-19
simple_title:         "处理 CSV 文件"

tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么？)
处理CSV就是读写以逗号分隔值格式存储的数据。程序员这么做因为CSV是交换表格数据的一个简单、通用的格式。

## How to (怎么做)
```TypeScript
// 引入文件系统模块和CSV解析库
import * as fs from 'fs';
import csvParse from 'csv-parse/lib/sync';

// 从CSV文件读取数据并解析
const csvData = fs.readFileSync('data.csv', 'utf8');
const parsedData = csvParse(csvData);

// 输出解析后的数据
console.log(parsedData);
```

输出示例：
```
[
  ['header1', 'header2', 'header3'],
  ['row1col1', 'row1col2', 'row1col3'],
  ['row2col1', 'row2col2', 'row2col3'],
  // ...
]
```

```TypeScript
// 引入文件系统模块和CSV字符串化库
import * as fs from 'fs';
import csvStringify from 'csv-stringify/lib/sync';

// 描述要写入的数据
const records = [
  { name: 'John', age: '30', job: 'Developer' },
  { name: 'Jane', age: '25', job: 'Designer' }
];

// 将数据转换为CSV格式
const csv = csvStringify(records, { header: true });

// 写入CSV文件
fs.writeFileSync('out.csv', csv);
```

输出文件(out.csv)：
```
name,age,job
John,30,Developer
Jane,25,Designer
```

## Deep Dive (深入了解)
- 历史背景：CSV格式源自1970年代初，早期电子表格软件开始使用。
- 替代方案：除了CSV，数据可以通过JSON、XML或数据库格式等其他方式交换。
- 实现细节：TypeScript处理CSV需要第三方库（如`csv-parse`和`csv-stringify`），因为原生不支持CSV格式。

## See Also (另请参见)
- Papa Parse: 具有强大功能的CSV解析库 [Papa Parse Github](https://github.com/mholt/PapaParse)
- csv-parse: 官方文档和更多示例 [csv-parse Documentation](https://csv.js.org/parse/)
- csv-stringify: 官方文档和更多示例 [csv-stringify Documentation](https://csv.js.org/stringify/)
- TypeScript基础知识和最佳实践 [TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/intro.html)
