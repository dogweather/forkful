---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:58.300727-07:00
description: "JSON(JavaScript Object Notation) \uCC98\uB9AC\uB294 JSON \uB370\uC774\
  \uD130\uB97C TypeScript\uC5D0\uC11C \uC0AC\uC6A9 \uAC00\uB2A5\uD55C \uD615\uC2DD\
  \uC73C\uB85C \uD30C\uC2F1\uD558\uB294 \uAC83\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB294 \uC774\uB97C \uD1B5\uD574 \uAD6C\uC870\uD654\
  \uB41C \uB370\uC774\uD130\uB97C \uC27D\uAC8C \uC870\uC791\uD558\uACE0, \uC800\uC7A5\
  \uD558\uACE0, \uC804\uC1A1\uD569\uB2C8\uB2E4. JSON\uC740 \uACBD\uB7C9\uC774\uBA70\
  , \uD14D\uC2A4\uD2B8 \uAE30\uBC18,\u2026"
lastmod: '2024-03-13T22:44:54.884056-06:00'
model: gpt-4-0125-preview
summary: "JSON(JavaScript Object Notation) \uCC98\uB9AC\uB294 JSON \uB370\uC774\uD130\
  \uB97C TypeScript\uC5D0\uC11C \uC0AC\uC6A9 \uAC00\uB2A5\uD55C \uD615\uC2DD\uC73C\
  \uB85C \uD30C\uC2F1\uD558\uB294 \uAC83\uC744 \uD3EC\uD568\uD569\uB2C8\uB2E4."
title: "JSON\uACFC \uD568\uAED8 \uC77C\uD558\uAE30"
weight: 38
---

## 무엇이며 왜?

JSON(JavaScript Object Notation) 처리는 JSON 데이터를 TypeScript에서 사용 가능한 형식으로 파싱하는 것을 포함합니다. 프로그래머는 이를 통해 구조화된 데이터를 쉽게 조작하고, 저장하고, 전송합니다. JSON은 경량이며, 텍스트 기반, 그리고 인간과 기계 모두에게 읽기 쉽기 때문입니다.

## 어떻게 하나:

### JSON을 TypeScript 객체로 파싱하기
JSON 문자열을 TypeScript 객체로 변환하려면 `JSON.parse()` 메서드를 사용합니다. 이는 웹 서버에서 JSON 데이터를 받거나 JSON 파일을 읽을 때 유용합니다.

```typescript
const jsonStr = '{"name": "John Doe", "age": 30}';
const obj = JSON.parse(jsonStr);

console.log(obj.name); // 출력: John Doe
```

### TypeScript 객체를 JSON 문자열로 변경하기
TypeScript 객체를 JSON 문자열로 변환하려면 `JSON.stringify()` 메서드를 사용합니다. 이는 웹 서버에 데이터를 보낼 필요가 있을 때 특히 유용합니다.

```typescript
const person = {
  name: "Jane Doe",
  age: 25,
};

const jsonStr = JSON.stringify(person);

console.log(jsonStr); // 출력: {"name":"Jane Doe","age":25}
```

### 인터페이스 사용하기
TypeScript 인터페이스를 정의하여 객체의 구조를 보장함으로써 JSON 데이터와 원활하게 작업할 수 있습니다.

```typescript
interface Person {
  name: string;
  age: number;
}

const jsonStr = '{"name": "Alex", "age": 28}';
const person: Person = JSON.parse(jsonStr);

console.log(person.age); // 출력: 28
```

### 인기 있는 타사 라이브러리 사용하기
스키마 검증이나 변환과 같이 더 복잡한 시나리오의 경우, `class-transformer` 또는 `AJV`(Another JSON Schema Validator)와 같은 라이브러리를 사용할 수 있습니다.

#### class-transformer
이 라이브러리는 일반 객체를 클래스 인스턴스로 변환하거나 그 반대로 할 수 있는데, 이는 타입 검사와 조작에 유용합니다.

```typescript
import "reflect-metadata";
import { plainToClass } from "class-transformer";
import { Person } from "./person";

const jsonStr = '{"name": "Mia", "age": 22}';
const person = plainToClass(Person, JSON.parse(jsonStr));

console.log(person instanceof Person); // 출력: true
console.log(person.name); // 출력: Mia
```

#### AJV
AJV는 빠른 JSON 스키마 검증을 가능하게 하는 라이브러리입니다. 이는 JSON 객체를 미리 정의된 스키마에 대응하여 검증할 수 있음을 의미합니다.

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

console.log(valid); // 출력: true
if (!valid) console.log(validate.errors);
```

이러한 도구와 기법을 사용함으로써, TypeScript 애플리케이션에서 JSON 데이터를 효율적으로 처리하면서 데이터의 무결성을 보장하고 TypeScript의 강력한 타입 시스템을 활용할 수 있습니다.
