---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:26.887828-07:00
description: "\u5982\u4F55\u64CD\u4F5C: \u8981\u5C06 JSON \u5B57\u7B26\u4E32\u8F6C\
  \u6362\u4E3A JavaScript \u5BF9\u8C61\uFF0C\u8BF7\u4F7F\u7528 `JSON.parse()`\u3002"
lastmod: '2024-04-05T21:53:48.515504-06:00'
model: gpt-4-0125-preview
summary: "\u8981\u5C06 JSON \u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A JavaScript \u5BF9\
  \u8C61\uFF0C\u8BF7\u4F7F\u7528 `JSON.parse()`\u3002"
title: "\u4F7F\u7528JSON\u8FDB\u884C\u7F16\u7A0B"
weight: 38
---

## 如何操作:


### 解析 JSON
要将 JSON 字符串转换为 JavaScript 对象，请使用 `JSON.parse()`。

```javascript
const jsonString = '{"name":"John", "age":30, "city":"New York"}';
const obj = JSON.parse(jsonString);
console.log(obj.name); // 输出：John
```

### 将 JavaScript 对象转换为 JSON 字符串
要将 JavaScript 对象转换回 JSON 字符串，请使用 `JSON.stringify()`。

```javascript
const user = { name: "Jane", age: 25, city: "London" };
const jsonString = JSON.stringify(user);
console.log(jsonString); // 输出：{"name":"Jane","age":25,"city":"London"}
```

### 在 Node.js 中处理文件
要在 Node.js 环境中读取 JSON 文件并将其转换为对象，可以使用 `fs` 模块。此示例假设你有一个名为 `data.json` 的文件。

```javascript
const fs = require('fs');

fs.readFile('data.json', 'utf-8', (err, data) => {
    if (err) throw err;
    const obj = JSON.parse(data);
    console.log(obj);
});
```

将对象写入 JSON 文件：

```javascript
const fs = require('fs');
const user = { name: "Mike", age: 22, city: "Berlin" };

fs.writeFile('user.json', JSON.stringify(user, null, 2), (err) => {
    if (err) throw err;
    console.log('数据已写入文件');
});
```

### 第三方库
对于复杂的 JSON 操作，像 `lodash` 这样的框架和库可以简化任务，但对于基本操作而言，原生 JavaScript 函数往往就足够了。对于大规模或性能至关重要的应用，你可以考虑使用像 `fast-json-stringify` 这样的库加速 JSON 字符串化，或使用 `json5` 用于解析和字符串化更灵活格式的 JSON。

使用 `json5` 解析：
```javascript
const JSON5 = require('json5');

const jsonString = '{name:"John", age:30, city:"New York"}';
const obj = JSON5.parse(jsonString);
console.log(obj.name); // 输出：John
```

这些示例涵盖了在 JavaScript 中与 JSON 进行基本操作，非常适合从其他语言过渡来的初学者，并希望在网络应用中有效地处理数据。
