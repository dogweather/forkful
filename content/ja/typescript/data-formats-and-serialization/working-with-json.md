---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:42.552560-07:00
description: "\u65B9\u6CD5\uFF1A JSON\u6587\u5B57\u5217\u3092TypeScript\u30AA\u30D6\
  \u30B8\u30A7\u30AF\u30C8\u306B\u5909\u63DB\u3059\u308B\u306B\u306F\u3001`JSON.parse()`\u30E1\
  \u30BD\u30C3\u30C9\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002\u3053\u308C\u306F\u3001\
  Web\u30B5\u30FC\u30D0\u30FC\u304B\u3089JSON\u30C7\u30FC\u30BF\u3092\u53D7\u4FE1\u3057\
  \u305F\u308A\u3001JSON\u30D5\u30A1\u30A4\u30EB\u3092\u8AAD\u307F\u8FBC\u3093\u3060\
  \u308A\u3059\u308B\u3068\u304D\u306B\u5F79\u7ACB\u3061\u307E\u3059\u3002"
lastmod: '2024-04-05T21:53:42.698815-06:00'
model: gpt-4-0125-preview
summary: ''
title: "JSON\u3092\u6D3B\u7528\u3059\u308B"
weight: 38
---

## 方法：


### JSONをTypeScriptオブジェクトに解析する
JSON文字列をTypeScriptオブジェクトに変換するには、`JSON.parse()`メソッドを使用します。これは、WebサーバーからJSONデータを受信したり、JSONファイルを読み込んだりするときに役立ちます。

```typescript
const jsonStr = '{"name": "John Doe", "age": 30}';
const obj = JSON.parse(jsonStr);

console.log(obj.name); // 出力：John Doe
```

### TypeScriptオブジェクトをJSONに文字列化する
TypeScriptオブジェクトをJSON文字列に変換するには、`JSON.stringify()`メソッドを使用します。これは、データをWebサーバーに送信する必要があるときに特に便利です。

```typescript
const person = {
  name: "Jane Doe",
  age: 25,
};

const jsonStr = JSON.stringify(person);

console.log(jsonStr); // 出力：{"name":"Jane Doe","age":25}
```

### インターフェースの利用
TypeScriptのインターフェースを定義して、オブジェクトの構造を保証することで、JSONデータとシームレスに作業できます。

```typescript
interface Person {
  name: string;
  age: number;
}

const jsonStr = '{"name": "Alex", "age": 28}';
const person: Person = JSON.parse(jsonStr);

console.log(person.age); // 出力：28
```

### 人気のあるサードパーティ製ライブラリの使用
スキーマ検証や変換など、より複雑なシナリオの場合、`class-transformer`や`AJV`（Another JSON Schema Validator）などのライブラリを利用することがあります。

#### class-transformer
このライブラリを使うと、プレーンオブジェクトをクラスインスタンスに変換したり、その逆をしたりできます。これは、型チェックや操作に役立ちます。

```typescript
import "reflect-metadata";
import { plainToClass } from "class-transformer";
import { Person } from "./person";

const jsonStr = '{"name": "Mia", "age": 22}';
const person = plainToClass(Person, JSON.parse(jsonStr));

console.log(person instanceof Person); // 出力：true
console.log(person.name); // 出力：Mia
```

#### AJV
AJVは、高速なJSONスキーマ検証を可能にするライブラリです。これは、事前に定義されたスキーマに対してJSONオブジェクトを検証できることを意味します。

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

console.log(valid); // 出力：true
if (!valid) console.log(validate.errors);
```

これらのツールとテクニックを利用することで、TypeScriptアプリケーションで効率的にJSONデータを扱うことができ、データの整合性を保ちながらTypeScriptの強力な型システムを活用できます。
