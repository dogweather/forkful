---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:05.065478-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.703955-06:00'
model: gpt-4-0125-preview
summary: "JavaScript\u3067CSV\uFF08\u30AB\u30F3\u30DE\u533A\u5207\u308A\u5024\uFF09\
  \u3092\u6271\u3046\u3068\u306F\u3001\u5916\u90E8\u30BD\u30FC\u30B9\u304B\u3089\u8868\
  \u5F62\u5F0F\u306E\u30C7\u30FC\u30BF\u3092\u53D6\u308A\u8FBC\u3093\u3060\u308A\u3001\
  \u4ED6\u306E\u30D7\u30ED\u30B0\u30E9\u30E0\u3067\u4F7F\u7528\u3059\u308B\u305F\u3081\
  \u306B\u30C7\u30FC\u30BF\u3092\u30A8\u30AF\u30B9\u30DD\u30FC\u30C8\u3059\u308B\u305F\
  \u3081\u306BCSV\u30D5\u30A1\u30A4\u30EB\u3092\u89E3\u6790\u307E\u305F\u306F\u751F\
  \u6210\u3059\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u304C\u3053\u308C\u3092\u884C\u3046\u7406\u7531\u306F\u3001\
  JSON\u306E\u3088\u3046\u306A\u3088\u308A\u8907\u96D1\u306A\u30D5\u30A9\u30FC\u30DE\
  \u30C3\u30C8\u304C\u904E\u5270\u3067\u3042\u308B\u5834\u5408\u306B\u3001\u30A2\u30D7\
  \u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u3001\u30C7\u30FC\u30BF\u30D9\u30FC\u30B9\u3001\
  \u30B7\u30B9\u30C6\u30E0\u9593\u3067\u306E\u30C7\u30FC\u30BF\u306E\u3084\u308A\u3068\
  \u308A\u3092\u7C21\u5358\u3067\u8EFD\u91CF\u306B\u3059\u308B\u305F\u3081\u3067\u3059\
  \u3002."
title: "CSV\u3068\u306E\u4F5C\u696D"
weight: 37
---

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
