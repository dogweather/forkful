---
title:                "处理CSV文件"
aliases: - /zh/javascript/working-with-csv.md
date:                  2024-02-03T19:20:41.197207-07:00
model:                 gpt-4-0125-preview
simple_title:         "处理CSV文件"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么和为什么？
在JavaScript中处理CSV（逗号分隔值）涉及解析或生成CSV文件，以便从外部源摄取表格数据或为其他程序导出数据使用。程序员这样做是因为它使得应用、数据库和系统之间的数据交换既简单又轻量，对于像JSON这样更复杂的格式可能是过度的。

## 如何做：
JavaScript没有内置的CSV解析或字符串化功能，就像它对待JSON那样。然而，你可以通过使用原始的JavaScript来处理简单任务，或利用像`PapaParse`这样强大的库来处理更复杂的场景，来轻松管理CSV数据。

### 使用原生JavaScript基础解析
要将一个简单的CSV字符串解析为对象数组：

```javascript
const csv = `name,age,city
John,23,New York
Jane,28,Los Angeles`;

function parseCSV(csv) {
  const lines = csv.split("\n");
  const result = [];
  const headers = lines[0].split(",");

  for (let i = 1; i < lines.length; i++) {
    const obj = {};
    const currentline = lines[i].split(",");
    
    for (let j = 0; j < headers.length; j++) {
      obj[headers[j]] = currentline[j];
    }
    result.push(obj);
  }
  
  return result;
}

console.log(parseCSV(csv));
```
输出：

```
[
  { name: 'John', age: '23', city: 'New York' },
  { name: 'Jane', age: '28', city: 'Los Angeles' }
]
```

### 使用原生JavaScript基础生成CSV
将一个对象数组转换为CSV字符串：

```javascript
const data = [
  { name: 'John', age: 23, city: 'New York' },
  { name: 'Jane', age: 28, city: 'Los Angeles' }
];

function arrayToCSV(arr) {
  const csv = arr.map(row => 
    Object.values(row).join(',')
  ).join('\n');
  
  return csv;
}

console.log(arrayToCSV(data));
```

输出：

```
John,23,New York
Jane,28,Los Angeles
```

### 使用PapaParse处理复杂的CSV任务
对于更复杂的场景，`PapaParse`是一个强大的库，适用于带有流、工作器和处理巨大文件选项的解析和字符串化CSV文件。

使用PapaParse解析CSV文件或字符串：

```javascript
// 添加PapaParse到你的项目之后
const Papa = require('papaparse');
const csv = `name,age,city
John,23,New York
Jane,28,Los Angeles`;

Papa.parse(csv, {
  complete: function(results) {
    console.log("Parsed:", results.data);
  }
});
```

生成：

```
Parsed: [
  ["name", "age", "city"],
  ["John", "23", "New York"],
  ["Jane", "28", "Los Angeles"]
]
```

使用PapaParse将数组字符串化为CSV字符串：

```javascript
const data = [
  { name: 'John', age: 23, city: 'New York' },
  { name: 'Jane', age: 28, city: 'Los Angeles' }
];

console.log(Papa.unparse(data));
```

生成：

```
name,age,city
John,23,New York
Jane,28,Los Angeles
```

这些示例展示了在JavaScript中处理CSV的基本和高级方法，使得在Web应用程序及更广泛的范围内的数据交换变得简单。
