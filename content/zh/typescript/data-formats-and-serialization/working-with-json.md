---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:29.242325-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u8981\u5C06 JSON \u5B57\u7B26\u4E32\u8F6C\
  \u6362\u4E3A TypeScript \u5BF9\u8C61\uFF0C\u60A8\u4F7F\u7528 `JSON.parse()` \u65B9\
  \u6CD5\u3002\u8FD9\u5728\u4ECE Web \u670D\u52A1\u5668\u63A5\u6536 JSON \u6570\u636E\
  \u6216\u8BFB\u53D6 JSON \u6587\u4EF6\u65F6\u5F88\u6709\u7528\u3002"
lastmod: '2024-04-05T21:53:47.819602-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u4F7F\u7528JSON\u8FDB\u884C\u7F16\u7A0B"
weight: 38
---

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
