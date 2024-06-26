---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:05.065478-07:00
description: ''
lastmod: '2024-04-05T22:50:56.573242-06:00'
model: gpt-4-0125-preview
summary: "JavaScript\u306FJSON\u3068\u306F\u7570\u306A\u308A\u3001CSV\u3092\u89E3\u6790\
  \u307E\u305F\u306F\u6587\u5B57\u5217\u5316\u3059\u308B\u7D44\u307F\u8FBC\u307F\u306E\
  \u6A5F\u80FD\u3092\u6301\u3063\u3066\u3044\u307E\u305B\u3093\u3002\u3057\u304B\u3057\
  \u3001\u3088\u308A\u30B7\u30F3\u30D7\u30EB\u306A\u30BF\u30B9\u30AF\u306B\u306F\u751F\
  \u306EJavaScript\u3092\u4F7F\u7528\u3059\u308B\u304B\u3001\u3088\u308A\u8907\u96D1\
  \u306A\u30B7\u30CA\u30EA\u30AA\u306B\u5BFE\u51E6\u3059\u308B\u305F\u3081\u306B`PapaParse`\u306E\
  \u3088\u3046\u306A\u5F37\u529B\u306A\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u5229\u7528\
  \u3059\u308B\u3053\u3068\u3067\u3001CSV\u30C7\u30FC\u30BF\u3092\u7C21\u5358\u306B\
  \u7BA1\u7406\u3067\u304D\u307E\u3059\u3002"
title: "CSV\u3068\u306E\u4F5C\u696D"
weight: 37
---

## 方法:
JavaScriptはJSONとは異なり、CSVを解析または文字列化する組み込みの機能を持っていません。しかし、よりシンプルなタスクには生のJavaScriptを使用するか、より複雑なシナリオに対処するために`PapaParse`のような強力なライブラリを利用することで、CSVデータを簡単に管理できます。

### 生のJavaScriptによる基本的な解析
単純なCSV文字列をオブジェクトの配列に解析するには：

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
出力:

```
[
  { name: 'John', age: '23', city: 'New York' },
  { name: 'Jane', age: '28', city: 'Los Angeles' }
]
```

### 生のJavaScriptによるCSVへの基本的な生成
オブジェクトの配列をCSV文字列に変換するには：

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

出力:

```
John,23,New York
Jane,28,Los Angeles
```

### 複雑なCSVタスクのためのPapaParseの使用
より複雑なシナリオのために、`PapaParse`はストリーム、ワーカー、巨大なファイルの取り扱いのオプションを備えた、解析と文字列化のための頑強なライブラリです。

PapaParseを使用したCSVファイルまたは文字列の解析：

```javascript
// プロジェクトにPapaParseを追加した後
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

生成される内容：

```
Parsed: [
  ["name", "age", "city"],
  ["John", "23", "New York"],
  ["Jane", "28", "Los Angeles"]
]
```

配列をCSV文字列に文字列化するPapaParseの使用：

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

これらの例は、JavaScriptにおける基本的および高度なCSVの取り扱いを示しており、ウェブアプリケーションやそれ以外の場所でのデータ交換を容易にします。
