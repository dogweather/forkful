---
title:                "JSONを活用する"
date:                  2024-02-03T19:24:42.552560-07:00
model:                 gpt-4-0125-preview
simple_title:         "JSONを活用する"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/typescript/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

JSON（JavaScript Object Notation）を使う作業は、JSONデータをTypeScriptで扱える形式に解析したり、その逆をしたりすることを含みます。プログラマーはこれを行うことで、構造化されたデータを簡単に操作、保存、または送信できます。なぜなら、JSONは軽量で、テキストベースであり、人間と機械の両方にとって読みやすいからです。

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
