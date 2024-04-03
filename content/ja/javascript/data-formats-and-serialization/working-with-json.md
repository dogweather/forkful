---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:28.085725-07:00
description: "JSON (JavaScript Object Notation)\u2026"
lastmod: '2024-03-13T22:44:42.703201-06:00'
model: gpt-4-0125-preview
summary: "JSON (JavaScript Object Notation) \u306F\u3001\u8EFD\u91CF\u306A\u30C7\u30FC\
  \u30BF\u4EA4\u63DB\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3067\u3059\u3002\u4EBA\u9593\
  \u304C\u8AAD\u307F\u66F8\u304D\u3057\u3084\u3059\u304F\u3001\u30DE\u30B7\u30F3\u304C\
  \u89E3\u6790\u3057\u751F\u6210\u3059\u308B\u306E\u306B\u9069\u3057\u3066\u3044\u307E\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001Web\u30A2\u30D7\u30EA\
  \u30B1\u30FC\u30B7\u30E7\u30F3\u3067\u30C7\u30FC\u30BF\u3092\u4FDD\u5B58\u30FB\u8EE2\
  \u9001\u3059\u308B\u305F\u3081\u306BJSON\u3092\u4F7F\u7528\u3057\u3001\u3053\u308C\
  \u304C\u73FE\u4EE3\u306EAPI\u304A\u3088\u3073Web\u30B5\u30FC\u30D3\u30B9\u901A\u4FE1\
  \u306E\u57FA\u76E4\u3068\u306A\u3063\u3066\u3044\u307E\u3059\u3002."
title: "JSON\u3092\u6D3B\u7528\u3059\u308B"
weight: 38
---

## どのようにして：


### JSONの解析
JSON文字列をJavaScriptオブジェクトに変換するには、`JSON.parse()`を使用します。

```javascript
const jsonString = '{"name":"John", "age":30, "city":"New York"}';
const obj = JSON.parse(jsonString);
console.log(obj.name); // 出力: John
```

### JavaScriptオブジェクトの文字列化
JavaScriptオブジェクトをJSON文字列に変換するには、`JSON.stringify()`を使用します。

```javascript
const user = { name: "Jane", age: 25, city: "London" };
const jsonString = JSON.stringify(user);
console.log(jsonString); // 出力: {"name":"Jane","age":25,"city":"London"}
```

### Node.jsでのファイル操作
Node.js環境でJSONファイルを読み込んでオブジェクトに変換するには、`fs`モジュールを使用できます。ここでは、`data.json`という名前のファイルがあることを前提としています。

```javascript
const fs = require('fs');

fs.readFile('data.json', 'utf-8', (err, data) => {
    if (err) throw err;
    const obj = JSON.parse(data);
    console.log(obj);
});
```

オブジェクトをJSONファイルに書き込むには：

```javascript
const fs = require('fs');
const user = { name: "Mike", age: 22, city: "Berlin" };

fs.writeFile('user.json', JSON.stringify(user, null, 2), (err) => {
    if (err) throw err;
    console.log('ファイルにデータが書き込まれました');
});
```

### サードパーティライブラリ
複雑なJSON操作には、`lodash`のようなフレームワークやライブラリがタスクを簡素化することができますが、基本的な操作のためには、ネイティブのJavaScript関数がしばしば十分です。大規模またはパフォーマンスが重要なアプリケーションには、より高速なJSON文字列化のための`fast-json-stringify`や、より柔軟なJSONフォーマットで解析・文字列化を行う`json5`のようなライブラリを検討することができます。

`json5`での解析：
```javascript
const JSON5 = require('json5');

const jsonString = '{name:"John", age:30, city:"New York"}';
const obj = JSON5.parse(jsonString);
console.log(obj.name); // 出力: John
```

これらの例は、JavaScriptでJSONを基本的に操作する方法をカバーしています。他の言語から移行してきた初心者が、Webアプリケーションで効率的にデータを扱うための完璧なガイドとなります。
