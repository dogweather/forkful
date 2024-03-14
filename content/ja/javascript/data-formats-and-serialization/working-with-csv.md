---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:05.065478-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.703955-06:00'
model: gpt-4-0125-preview
summary: "\u2026"
title: "CSV\u3068\u306E\u4F5C\u696D"
---

{{< edit_this_page >}}

## はじめに - 何となぜ？
JavaScriptでCSV（カンマ区切り値）を扱うとは、外部ソースから表形式のデータを取り込んだり、他のプログラムで使用するためにデータをエクスポートするためにCSVファイルを解析または生成することを意味します。プログラマーがこれを行う理由は、JSONのようなより複雑なフォーマットが過剰である場合に、アプリケーション、データベース、システム間でのデータのやりとりを簡単で軽量にするためです。

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
