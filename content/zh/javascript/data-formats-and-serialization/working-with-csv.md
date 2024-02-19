---
aliases:
- /zh/javascript/working-with-csv/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:41.197207-07:00
description: "\u5728JavaScript\u4E2D\u5904\u7406CSV\uFF08\u9017\u53F7\u5206\u9694\u503C\
  \uFF09\u6D89\u53CA\u89E3\u6790\u6216\u751F\u6210CSV\u6587\u4EF6\uFF0C\u4EE5\u4FBF\
  \u4ECE\u5916\u90E8\u6E90\u6444\u53D6\u8868\u683C\u6570\u636E\u6216\u4E3A\u5176\u4ED6\
  \u7A0B\u5E8F\u5BFC\u51FA\u6570\u636E\u4F7F\u7528\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\
  \u505A\u662F\u56E0\u4E3A\u5B83\u4F7F\u5F97\u5E94\u7528\u3001\u6570\u636E\u5E93\u548C\
  \u7CFB\u7EDF\u4E4B\u95F4\u7684\u6570\u636E\u4EA4\u6362\u65E2\u7B80\u5355\u53C8\u8F7B\
  \u91CF\uFF0C\u5BF9\u4E8E\u50CFJSON\u8FD9\u6837\u66F4\u590D\u6742\u7684\u683C\u5F0F\
  \u53EF\u80FD\u662F\u8FC7\u5EA6\u7684\u3002"
lastmod: 2024-02-18 23:08:59.499789
model: gpt-4-0125-preview
summary: "\u5728JavaScript\u4E2D\u5904\u7406CSV\uFF08\u9017\u53F7\u5206\u9694\u503C\
  \uFF09\u6D89\u53CA\u89E3\u6790\u6216\u751F\u6210CSV\u6587\u4EF6\uFF0C\u4EE5\u4FBF\
  \u4ECE\u5916\u90E8\u6E90\u6444\u53D6\u8868\u683C\u6570\u636E\u6216\u4E3A\u5176\u4ED6\
  \u7A0B\u5E8F\u5BFC\u51FA\u6570\u636E\u4F7F\u7528\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\
  \u505A\u662F\u56E0\u4E3A\u5B83\u4F7F\u5F97\u5E94\u7528\u3001\u6570\u636E\u5E93\u548C\
  \u7CFB\u7EDF\u4E4B\u95F4\u7684\u6570\u636E\u4EA4\u6362\u65E2\u7B80\u5355\u53C8\u8F7B\
  \u91CF\uFF0C\u5BF9\u4E8E\u50CFJSON\u8FD9\u6837\u66F4\u590D\u6742\u7684\u683C\u5F0F\
  \u53EF\u80FD\u662F\u8FC7\u5EA6\u7684\u3002"
title: "\u5904\u7406CSV\u6587\u4EF6"
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
