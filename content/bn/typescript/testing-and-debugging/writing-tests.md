---
title:                "টেস্ট লিখা"
date:                  2024-03-17T18:42:14.959134-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি ও কেন?
TypeScript এ টেস্ট লেখার অর্থ হল আপনার কোডের ফাংশনালিটি এবং সঠিকতা যাচাই করার জন্য স্বয়ংক্রিয় স্ক্রিপ্ট তৈরি করা। প্রোগ্রামাররা এটি নির্ভরযোগ্যতা নিশ্চিত করার জন্য, দ্রুতগতিতে বাগ ধরা এবং কোডের বার্ধক্য সহনীয় করার উদ্দেশ্যে করে থাকেন, যেহেতু TypeScript এর স্ট্যাটিক টাইপিং জাভাস্ক্রিপ্ট টেস্টিংকে এক ধরনের প্রেডিক্টেবিলিটি যোগ করে।

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
