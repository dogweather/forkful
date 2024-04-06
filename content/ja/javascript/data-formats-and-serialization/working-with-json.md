---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:28.085725-07:00
description: "\u3069\u306E\u3088\u3046\u306B\u3057\u3066\uFF1A JSON\u6587\u5B57\u5217\
  \u3092JavaScript\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u306B\u5909\u63DB\u3059\u308B\
  \u306B\u306F\u3001`JSON.parse()`\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.703201-06:00'
model: gpt-4-0125-preview
summary: "JSON\u6587\u5B57\u5217\u3092JavaScript\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\
  \u306B\u5909\u63DB\u3059\u308B\u306B\u306F\u3001`JSON.parse()`\u3092\u4F7F\u7528\
  \u3057\u307E\u3059."
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
