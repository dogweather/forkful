---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:30:10.458826-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u098F\u0995\u099F\u09BF JSON\
  \ \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\u09C7 TypeScript \u0985\u09AC\
  \u099C\u09C7\u0995\u09CD\u099F\u09C7 \u09AA\u09B0\u09BF\u09A3\u09A4 \u0995\u09B0\
  \u09A4\u09C7, \u0986\u09AA\u09A8\u09BF `JSON.parse()` \u09AE\u09C7\u09A5\u09A1 \u09AC\
  \u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09AC\u09C7\u09A8\u0964 \u098F\
  \u099F\u09BF \u09AC\u09BF\u09B6\u09C7\u09B7\u09A4 \u0989\u09AA\u0995\u09BE\u09B0\
  \u09C0 \u09B9\u09AF\u09BC \u09AF\u0996\u09A8 \u0986\u09AA\u09A8\u09BF \u098F\u0995\
  \u099F\u09BF \u0993\u09AF\u09BC\u09C7\u09AC \u09B8\u09BE\u09B0\u09CD\u09AD\u09BE\
  \u09B0 \u09A5\u09C7\u0995\u09C7\u2026"
lastmod: '2024-03-17T18:47:43.787263-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0995\u099F\u09BF JSON \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\
  \u0995\u09C7 TypeScript \u0985\u09AC\u099C\u09C7\u0995\u09CD\u099F\u09C7 \u09AA\u09B0\
  \u09BF\u09A3\u09A4 \u0995\u09B0\u09A4\u09C7, \u0986\u09AA\u09A8\u09BF `JSON.parse()`\
  \ \u09AE\u09C7\u09A5\u09A1 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09AC\u09C7\u09A8\u0964 \u098F\u099F\u09BF \u09AC\u09BF\u09B6\u09C7\u09B7\u09A4\
  \ \u0989\u09AA\u0995\u09BE\u09B0\u09C0 \u09B9\u09AF\u09BC \u09AF\u0996\u09A8 \u0986\
  \u09AA\u09A8\u09BF \u098F\u0995\u099F\u09BF \u0993\u09AF\u09BC\u09C7\u09AC \u09B8\
  \u09BE\u09B0\u09CD\u09AD\u09BE\u09B0 \u09A5\u09C7\u0995\u09C7 JSON \u09A1\u09BE\u099F\
  \u09BE \u0997\u09CD\u09B0\u09B9\u09A3 \u0995\u09B0\u09C7\u09A8 \u09AC\u09BE \u098F\
  \u0995\u099F\u09BF JSON \u09AB\u09BE\u0987\u09B2 \u09AA\u09A1\u09BC\u09C7\u09A8\u0964\
  ."
title: "JSON \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\
  \u09BE"
weight: 38
---

## কিভাবে:


### JSON কে TypeScript অবজেক্টে পার্স করা
একটি JSON স্ট্রিংকে TypeScript অবজেক্টে পরিণত করতে, আপনি `JSON.parse()` মেথড ব্যবহার করবেন। এটি বিশেষত উপকারী হয় যখন আপনি একটি ওয়েব সার্ভার থেকে JSON ডাটা গ্রহণ করেন বা একটি JSON ফাইল পড়েন।

```typescript
const jsonStr = '{"name": "John Doe", "age": 30}';
const obj = JSON.parse(jsonStr);

console.log(obj.name); // আউটপুট: John Doe
```

### একটি TypeScript অবজেক্টকে JSON স্ট্রিং এ স্ট্রিংগিফাই করা
একটি TypeScript অবজেক্টকে JSON স্ট্রিং এ পরিণত করতে, আপনি `JSON.stringify()` মেথড ব্যবহার করবেন। এটি বিশেষত উপকারী হয় যখন আপনার একটি ওয়েব সার্ভারে ডাটা পাঠানোর প্রয়োজন হয়।

```typescript
const person = {
  name: "Jane Doe",
  age: 25,
};

const jsonStr = JSON.stringify(person);

console.log(jsonStr); // আউটপুট: {"name":"Jane Doe","age":25}
```

### ইন্টারফেসের সাথে কাজ করা
আপনি TypeScript ইন্টারফেসগুলি ডিফাইন করে JSON ডাটার সাথে সহজেই কাজ করতে পারেন আপনার অবজেক্টগুলির গঠন নিশ্চিত করে।

```typescript
interface Person {
  name: string;
  age: number;
}

const jsonStr = '{"name": "Alex", "age": 28}';
const person: Person = JSON.parse(jsonStr);

console.log(person.age); // আউটপুট: 28
```

### জনপ্রিয় থার্ড-পার্টি লাইব্রেরীগুলি ব্যবহার করা
স্কিমা ভ্যালিডেশন বা রূপান্তরের মতো আরও জটিল পরিস্থিতিগুলির জন্য, আপনি `class-transformer` বা `AJV` (Another JSON Schema Validator) এর মতো লাইব্রেরীগুলিতে অবলম্বন করতে পারেন।

#### class-transformer
এই লাইব্রেরি সাধারণ অবজেক্টগুলিকে ক্লাস ইন্সট্যান্সে পরিণত করতে এবং উল্টোদিকে করতে সক্ষম, যা টাইপ চেকিং এবং ম্যানিপুলেশনের জন্য উপকারী।

```typescript
import "reflect-metadata";
import { plainToClass } from "class-transformer";
import { Person } from "./person";

const jsonStr = '{"name": "Mia", "age": 22}';
const person = plainToClass(Person, JSON.parse(jsonStr));

console.log(person instanceof Person); // আউটপুট: true
console.log(person.name); // আউটপুট: Mia
```

#### AJV
AJV একটি লাইব্রেরি যা দ্রুত JSON স্কিমা ভ্যালিডেশন অনুমোদন করে। এর মানে হচ্ছে আপনি প্রি-ডিফাইনড স্কিমার বিরুদ্ধে JSON অবজেক্টগুলি ভ্যালিডেট করতে পারেন।

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

console.log(valid); // আউটপুট: true
if (!valid) console.log(validate.errors);
```

এই সরঞ্জাম এবং প্রযুক্তিগুলির সাহায্যে, আপনি আপনার TypeScript অ্যাপ্লিকেশনগুলিতে JSON ডাটা দক্ষভাবে হ্যান্ডল করতে পারবেন, ডাটা ইন্টিগ্রিটি নিশ্চিত করতে পারবেন এবং TypeScript এর শক্তিশালী টাইপ সিস্টেমের সুবিধা নিতে পারবেন।
