---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:29.242325-07:00
description: "\u4F7F\u7528 JSON\uFF08JavaScript \u5BF9\u8C61\u8868\u793A\u6CD5\uFF09\
  \u6D89\u53CA\u5C06 JSON \u6570\u636E\u89E3\u6790\u4E3A TypeScript \u4E2D\u53EF\u7528\
  \u7684\u683C\u5F0F\uFF0C\u5E76\u4ECE\u8BE5\u683C\u5F0F\u89E3\u6790\u51FA\u6765\u3002\
  \u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u8F7B\u677E\u5730\u64CD\u4F5C\
  \u3001\u5B58\u50A8\u6216\u4F20\u8F93\u7ED3\u6784\u5316\u6570\u636E\uFF0C\u56E0\u4E3A\
  \ JSON \u662F\u8F7B\u91CF\u7EA7\u7684\u3001\u57FA\u4E8E\u6587\u672C\u7684\uFF0C\u5E76\
  \u4E14\u5BF9\u4EBA\u7C7B\u548C\u673A\u5668\u90FD\u6613\u4E8E\u9605\u8BFB\u3002"
lastmod: 2024-02-19 22:05:06.513323
model: gpt-4-0125-preview
summary: "\u4F7F\u7528 JSON\uFF08JavaScript \u5BF9\u8C61\u8868\u793A\u6CD5\uFF09\u6D89\
  \u53CA\u5C06 JSON \u6570\u636E\u89E3\u6790\u4E3A TypeScript \u4E2D\u53EF\u7528\u7684\
  \u683C\u5F0F\uFF0C\u5E76\u4ECE\u8BE5\u683C\u5F0F\u89E3\u6790\u51FA\u6765\u3002\u7A0B\
  \u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u8F7B\u677E\u5730\u64CD\u4F5C\u3001\
  \u5B58\u50A8\u6216\u4F20\u8F93\u7ED3\u6784\u5316\u6570\u636E\uFF0C\u56E0\u4E3A JSON\
  \ \u662F\u8F7B\u91CF\u7EA7\u7684\u3001\u57FA\u4E8E\u6587\u672C\u7684\uFF0C\u5E76\
  \u4E14\u5BF9\u4EBA\u7C7B\u548C\u673A\u5668\u90FD\u6613\u4E8E\u9605\u8BFB\u3002"
title: "\u4F7F\u7528JSON\u8FDB\u884C\u7F16\u7A0B"
---

{{< edit_this_page >}}

## 什么 & 为什么？

使用 JSON（JavaScript 对象表示法）涉及将 JSON 数据解析为 TypeScript 中可用的格式，并从该格式解析出来。程序员这样做是为了轻松地操作、存储或传输结构化数据，因为 JSON 是轻量级的、基于文本的，并且对人类和机器都易于阅读。

## 如何操作：

### 将 JSON 解析为 TypeScript 对象
要将 JSON 字符串转换为 TypeScript 对象，您使用 `JSON.parse()` 方法。这在从 Web 服务器接收 JSON 数据或读取 JSON 文件时很有用。

```typescript
const jsonStr = '{"name": "John Doe", "age": 30}';
const obj = JSON.parse(jsonStr);

console.log(obj.name); // 输出: John Doe
```

### 将 TypeScript 对象字符串化为 JSON
要将 TypeScript 对象转换为 JSON 字符串，你使用 `JSON.stringify()` 方法。这在您需要将数据发送到 Web 服务器时特别有用。

```typescript
const person = {
  name: "Jane Doe",
  age: 25,
};

const jsonStr = JSON.stringify(person);

console.log(jsonStr); // 输出: {"name":"Jane Doe","age":25}
```

### 使用接口
您可以定义 TypeScript 接口以通过确保您的对象结构与 JSON 数据无缝工作。

```typescript
interface Person {
  name: string;
  age: number;
}

const jsonStr = '{"name": "Alex", "age": 28}';
const person: Person = JSON.parse(jsonStr);

console.log(person.age); // 输出: 28
```

### 使用流行的第三方库
对于更复杂的场景，如模式验证或转换，您可能会使用像 `class-transformer` 或 `AJV`（Another JSON Schema Validator）这样的库。

#### class-transformer
这个库可以将平面对象转换为类实例，反之亦然，这对于类型检查和操作很有用。

```typescript
import "reflect-metadata";
import { plainToClass } from "class-transformer";
import { Person } from "./person";

const jsonStr = '{"name": "Mia", "age": 22}';
const person = plainToClass(Person, JSON.parse(jsonStr));

console.log(person instanceof Person); // 输出: true
console.log(person.name); // 输出: Mia
```

#### AJV
AJV 是一个允许进行快速 JSON 模式验证的库。这意味着您可以根据预定义的模式验证 JSON 对象。

```typescript
import Ajv from "ajv";

const ajv = new Ajv();

const schema = {
  type: "object",
  properties: {
    name: { type: "string" },
    age: { type: "number" },
  },
  required: ["name", "age"],
  additionalProperties: false,
};

const validate = ajv.compile(schema);
const valid = validate({ name: "Tom", age: 24 });

console.log(valid); // 输出: true
if (!valid) console.log(validate.errors);
```

通过这些工具和技术，您可以在 TypeScript 应用程序中有效处理 JSON 数据，确保数据完整性并利用 TypeScript 强大的类型系统。
