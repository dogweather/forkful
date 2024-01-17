---
title:                "使用csv进行编程"
html_title:           "Javascript: 使用csv进行编程"
simple_title:         "使用csv进行编程"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/working-with-csv.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

CSV（逗号分隔值）是一种用来存储表格数据的文件格式，通常由逗号或者分号来分隔不同的数据项。程序员们经常要处理CSV文件，是因为它们提供了一种简单，易于读取和写入的格式来存储数据，并且可以轻松地与其他编程语言进行交互。

## 如何：
```
// 1. 读取CSV文件
const csv = require('csv-parser');
const fs = require('fs');

fs.createReadStream('file.csv')
  .pipe(csv())
  .on('data', (row) => {
  // 处理每一行数据
    console.log(row);
  })
  .on('end', () => {
  // 结束时打印完成
    console.log('读取完成');
  });

// 2. 写入CSV文件
const createCsvWriter = require('csv-writer').createObjectCsvWriter;

const csvWriter = createCsvWriter({
  path: 'file.csv',
  header: [
    {id: 'name', title: '姓名'},
    {id: 'age', title: '年龄'}
  ]
});

const records = [
  {name: '张三', age: 25},
  {name: '李四', age: 30},
  {name: '王五', age: 35}
];

csvWriter
  .writeRecords(records)
  .then(() => {
    console.log('CSV文件写入完成');
  })

```

输出:
```
{ name: '张三', age: '25' }
{ name: '李四', age: '30' }
{ name: '王五', age: '35' }
CSV文件写入完成
```

## 深入了解：
CSV文件最早是由美国的微软和IBM公司共同开发出来，它的主要用途是在电子表格软件中导入和导出数据。但是随着互联网和网络技术的发展，CSV文件也被越来越多的程序员用来存储和传输数据。除了逗号和分号之外，CSV文件还可以用其他符号来分隔数据，比如制表符、空格等。此外，除了使用库来读取和写入CSV文件外，我们也可以手动编写代码来处理CSV文件，这需要一些额外的步骤和技巧。

## 参考链接：
- [CSV文件格式简介](https://baike.baidu.com/item/CSV/10709?fr=aladdin)
- [csv-parser库官方文档](https://www.npmjs.com/package/csv-parser)
- [csv-writer库官方文档](https://www.npmjs.com/package/csv-writer)