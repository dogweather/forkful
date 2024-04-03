---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:42:14.959134-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: TypeScript \u09AC\u09C7\u09B6\u09BF\
  \u09B0\u09AD\u09BE\u0997 \u099C\u09BE\u09AD\u09BE\u09B8\u09CD\u0995\u09CD\u09B0\u09BF\
  \u09AA\u09CD\u099F \u099F\u09C7\u09B8\u09CD\u099F\u09BF\u0982 \u09AB\u09CD\u09B0\
  \u09C7\u09AE\u0993\u09AF\u09BC\u09BE\u09B0\u09CD\u0995\u09C7\u09B0 \u09B8\u09BE\u09A5\
  \u09C7 \u09B8\u09C1\u09B8\u0982\u09B9\u09A4\u09AD\u09BE\u09AC\u09C7 \u0995\u09BE\
  \u099C \u0995\u09B0\u09C7\u0964 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3 \u09B8\u09CD\
  \u09AC\u09B0\u09C2\u09AA, \u0986\u09AE\u09B0\u09BE Jest, \u098F\u0995\u099F\u09BF\
  \ \u099C\u09A8\u09AA\u09CD\u09B0\u09BF\u09AF\u09BC \u099F\u09C7\u09B8\u09CD\u099F\
  \u09BF\u0982 \u09AB\u09CD\u09B0\u09C7\u09AE\u0993\u09AF\u09BC\u09BE\u09B0\u09CD\u0995\
  \u2026"
lastmod: '2024-03-17T18:47:43.768707-06:00'
model: gpt-4-0125-preview
summary: "TypeScript \u09AC\u09C7\u09B6\u09BF\u09B0\u09AD\u09BE\u0997 \u099C\u09BE\
  \u09AD\u09BE\u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AA\u09CD\u099F \u099F\u09C7\u09B8\
  \u09CD\u099F\u09BF\u0982 \u09AB\u09CD\u09B0\u09C7\u09AE\u0993\u09AF\u09BC\u09BE\u09B0\
  \u09CD\u0995\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u09B8\u09C1\u09B8\u0982\u09B9\
  \u09A4\u09AD\u09BE\u09AC\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09C7\u0964 \u0989\
  \u09A6\u09BE\u09B9\u09B0\u09A3 \u09B8\u09CD\u09AC\u09B0\u09C2\u09AA, \u0986\u09AE\
  \u09B0\u09BE Jest, \u098F\u0995\u099F\u09BF \u099C\u09A8\u09AA\u09CD\u09B0\u09BF\
  \u09AF\u09BC \u099F\u09C7\u09B8\u09CD\u099F\u09BF\u0982 \u09AB\u09CD\u09B0\u09C7\
  \u09AE\u0993\u09AF\u09BC\u09BE\u09B0\u09CD\u0995 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09AC, \u0995\u09BE\u09B0\u09A3 \u098F\u099F\u09BF TypeScript\
  \ \u09AA\u09CD\u09B0\u09CB\u099C\u09C7\u0995\u09CD\u099F\u09C7\u09B0 \u099C\u09A8\
  \u09CD\u09AF \u09B6\u09C2\u09A8\u09CD\u09AF-\u0995\u09A8\u09AB\u09BF\u0997\u09BE\
  \u09B0\u09C7\u09B6\u09A8 \u09B8\u09C7\u099F\u0986\u09AA \u09AA\u09CD\u09B0\u09A6\
  \u09BE\u09A8 \u0995\u09B0\u09C7\u0964\n\n\u09AA\u09CD\u09B0\u09A5\u09AE\u09C7, \u09A8\
  \u09BF\u09B6\u09CD\u099A\u09BF\u09A4 \u0995\u09B0\u09C1\u09A8 \u09AF\u09C7 \u0986\
  \u09AA\u09A8\u09BE\u09B0 \u0995\u09BE\u099B\u09C7 Jest \u098F\u09AC\u0982 \u09AA\
  \u09CD\u09B0\u09AF\u09BC\u09CB\u099C\u09A8\u09C0\u09AF\u09BC TypeScript \u099F\u09BE\
  \u0987\u09AA\u0997\u09C1\u09B2\u09BF \u0987\u09A8\u09B8\u09CD\u099F\u09B2 \u0995\
  \u09B0\u09BE \u0986\u099B\u09C7."
title: "\u099F\u09C7\u09B8\u09CD\u099F \u09B2\u09BF\u0996\u09BE"
weight: 36
---

## কিভাবে:
TypeScript বেশিরভাগ জাভাস্ক্রিপ্ট টেস্টিং ফ্রেমওয়ার্কের সাথে সুসংহতভাবে কাজ করে। উদাহরণ স্বরূপ, আমরা Jest, একটি জনপ্রিয় টেস্টিং ফ্রেমওয়ার্ক ব্যবহার করব, কারণ এটি TypeScript প্রোজেক্টের জন্য শূন্য-কনফিগারেশন সেটআপ প্রদান করে।

প্রথমে, নিশ্চিত করুন যে আপনার কাছে Jest এবং প্রয়োজনীয় TypeScript টাইপগুলি ইনস্টল করা আছে:

```bash
npm install --save-dev jest typescript ts-jest @types/jest
```

পরবর্তীতে, TypeScript এর সাথে কাজ করার জন্য Jest সেটআপ করুন এটি `jest.config.js` পরিবর্তন করে অথবা নতুন একটি তৈরি করে:

```javascript
module.exports = {
  preset: 'ts-jest',
  testEnvironment: 'node',
};
```

এখন, চলুন একটি সহজ ফাংশন এবং এর জন্য একটি টেস্ট লিখি। `sum.ts` ফাইলটি নিচের ফাংশনটি বিবেচনা করুন:

```typescript
// sum.ts
export function sum(a: number, b: number): number {
  return a + b;
}
```

`sum.test.ts` নামে একটি টেস্ট ফাইল তৈরি করুন:

```typescript
// sum.test.ts
import { sum } from './sum';

test('adds 1 + 2 to equal 3', () => {
  expect(sum(1, 2)).toBe(3);
});
```

আপনার টেস্ট চালাতে:

```bash
npx jest
```

পাস করা টেস্টের নমুনা আউটপুট দেখতে হবে কিছু এরকম:

```plaintext
 PASS  ./sum.test.ts
  ✓ adds 1 + 2 to equal 3 (2 ms)
```

অ্যাসিনক্রোনাস কোডের জন্য, Jest `async/await` এর মাধ্যমে সমর্থন করে। ধরুন আপনার কাছে একটি অ্যাসিনক্রোনাস `fetchData` ফাংশন আছে:

```typescript
// asyncFunctions.ts
export async function fetchData(): Promise<string> {
  return "data";
}
```

আপনার অ্যাসিনক্রোনাস ফাংশনকে ব্যবহার করে টেস্ট:

```typescript
// asyncFunctions.test.ts
import { fetchData } from './asyncFunctions';

test('fetches data successfully', async () => {
  expect(await fetchData()).toBe('data');
});
```

আপনার টেস্ট চালানোর সময়, Jest প্রমিসটি রেজলভ হওয়া অবধি অপেক্ষা করবে, সঠিকভাবে অ্যাসিনক্রোনাস অপারেশনগুলি টেস্ট করে।

মনে রাখবেন, কার্যকর টেস্টিং বিভিন্ন সিনারিও এবং প্রান্তিক কেসগুলি সহ একাধিক টেস্ট লেখা অন্তর্ভুক্ত, যাতে আপনার TypeScript কোড প্রত্যাশিত আচরণ করে।
