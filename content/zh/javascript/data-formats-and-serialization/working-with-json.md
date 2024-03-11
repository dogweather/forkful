---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:23:26.887828-07:00
description: "JSON\uFF08JavaScript \u5BF9\u8C61\u8868\u793A\u6CD5\uFF09\u662F\u4E00\
  \u79CD\u8F7B\u91CF\u7EA7\u7684\u6570\u636E\u4EA4\u6362\u683C\u5F0F\uFF0C\u6613\u4E8E\
  \u4EBA\u9605\u8BFB\u548C\u7F16\u5199\uFF0C\u4E5F\u4FBF\u4E8E\u673A\u5668\u89E3\u6790\
  \u548C\u751F\u6210\u3002\u7A0B\u5E8F\u5458\u7528\u5B83\u6765\u5B58\u50A8\u548C\u4F20\
  \u8F93\u7F51\u7EDC\u5E94\u7528\u4E2D\u7684\u6570\u636E\uFF0C\u4F7F\u5176\u6210\u4E3A\
  \u73B0\u4EE3 API \u548C\u7F51\u7EDC\u670D\u52A1\u901A\u4FE1\u7684\u652F\u67F1\u3002"
lastmod: '2024-03-11T00:14:22.045544-06:00'
model: gpt-4-0125-preview
summary: "JSON\uFF08JavaScript \u5BF9\u8C61\u8868\u793A\u6CD5\uFF09\u662F\u4E00\u79CD\
  \u8F7B\u91CF\u7EA7\u7684\u6570\u636E\u4EA4\u6362\u683C\u5F0F\uFF0C\u6613\u4E8E\u4EBA\
  \u9605\u8BFB\u548C\u7F16\u5199\uFF0C\u4E5F\u4FBF\u4E8E\u673A\u5668\u89E3\u6790\u548C\
  \u751F\u6210\u3002\u7A0B\u5E8F\u5458\u7528\u5B83\u6765\u5B58\u50A8\u548C\u4F20\u8F93\
  \u7F51\u7EDC\u5E94\u7528\u4E2D\u7684\u6570\u636E\uFF0C\u4F7F\u5176\u6210\u4E3A\u73B0\
  \u4EE3 API \u548C\u7F51\u7EDC\u670D\u52A1\u901A\u4FE1\u7684\u652F\u67F1\u3002"
title: "\u4F7F\u7528JSON\u8FDB\u884C\u7F16\u7A0B"
---

{{< edit_this_page >}}

## 什么 & 为什么?

JSON（JavaScript 对象表示法）是一种轻量级的数据交换格式，易于人阅读和编写，也便于机器解析和生成。程序员用它来存储和传输网络应用中的数据，使其成为现代 API 和网络服务通信的支柱。

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
