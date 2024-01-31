---
title:                "处理 CSV 文件"
date:                  2024-01-19
html_title:           "Bash: 处理 CSV 文件"
simple_title:         "处理 CSV 文件"

category:             "Javascript"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
什么是CSV，为什么要用？CSV（逗号分隔值）是存储表格数据（比如从Excel导出的数据）的一种简单格式。程序员操作CSV因为易于阅读，简单编辑，且广泛兼容各种程序。

## How to:
JavaScript代码示例和输出结果。

```javascript
// 如何读取CSV文件
const fs = require('fs');
const parse = require('csv-parse/lib/sync');

const csvData = fs.readFileSync('example.csv', 'utf8');
const records = parse(csvData, {
  columns: true,
  skip_empty_lines: true
});

console.log(records);
```

```javascript
// 如何写入CSV文件
const fs = require('fs');
const stringify = require('csv-stringify');

const data = [
    { name: "张三", age: 28, city: "北京" },
    { name: "李四", age: 35, city: "上海" }
];

stringify(data, {
    header: true
}, (err, output) => {
    if (err) throw err;
    fs.writeFileSync('output.csv', output);
});
```

输出示例：

```
[ { name: '张三', age: '28', city: '北京' },
  { name: '李四', age: '35', city: '上海' } ]
```

## Deep Dive
CSV起源于20世纪早期，当时商业和科学领域需要一种结构化数据的简单形式。今天，JSON和XML是CSV的常见替代品，提供了更复杂的数据序列化方式。CSV在JavaScript中的处理细节包括正确解析数据、处理不同的分隔符以及转义字符等等。

## See Also
- CSV标准: [RFC 4180](https://tools.ietf.org/html/rfc4180)
- Node.js中的`csv-parse`库: [csv-parse](https://www.npmjs.com/package/csv-parse)
- Node.js中的`csv-stringify`库: [csv-stringify](https://www.npmjs.com/package/csv-stringify)
- 更多关于JSON的资料: [MDN JSON](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/JSON)
- 关于XML的进一步理解: [XML 教程](https://www.w3school.com.cn/xml/index.asp)
