---
title:                "处理JSON数据"
html_title:           "Arduino: 处理JSON数据"
simple_title:         "处理JSON数据"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (什么与为什么？)
JSON（JavaScript Object Notation）是数据交换的标准格式。它被广泛使用，因为它易于阅读且与多种编程语言兼容，包括TypeScript。

## How to: (如何做：)
```TypeScript
// 解析JSON字符串到对象
const jsonString: string = '{"name": "张三", "age": 30}';
const user = JSON.parse(jsonString);
console.log(user); // { name: '张三', age: 30 }

// 将对象转换成JSON字符串
const userObject = { name: '李四', age: 25 };
const userString: string = JSON.stringify(userObject);
console.log(userString); // '{"name":"李四","age":25}'
```

## Deep Dive (深入探索)
JSON自2001年被引入以来，快速成为了Web API通信的主流。虽然还有XML等替代格式，JSON因其简洁性和高效性成为了首选。在TypeScript中，你可以通过泛型来定义JSON结构，以确保类型安全。

## See Also (另请参阅)
- [MDN Web Docs上的JSON](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/JSON)
- [TypeScript Handbook（TypeScript手册）](https://www.typescriptlang.org/docs/handbook/intro.html)
- [Understanding JSON (理解JSON)](https://www.json.org/json-zh.html)
