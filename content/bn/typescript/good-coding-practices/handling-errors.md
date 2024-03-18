---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:51:10.350884-06:00
description: "\u09AD\u09C1\u09B2 \u09B8\u09AE\u09CD\u09AA\u09B0\u09CD\u0995\u09C7\
  \ \u09B9\u09BE\u09A4\u09BE\u09B2\u09C7\u09B0 \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2\
  \ \u0985\u09A8\u09BE\u0995\u09BE\u0999\u09CD\u0995\u09CD\u09B7\u09BF\u09A4 \u0998\
  \u099F\u09A8\u09BE\u0995\u09C7 \u0986\u09B6\u09BE \u0995\u09B0\u09BE; \u0986\u09AE\
  \u09BE\u09A6\u09C7\u09B0 \u0995\u09CB\u09A1\u09C7 \u09AF\u0996\u09A8 \u0995\u09BF\
  \u099B\u09C1 \u09AD\u09C1\u09B2 \u0998\u099F\u09C7, \u09A4\u09BE \u0995\u09BF\u09AD\
  \u09BE\u09AC\u09C7 \u09B8\u09BE\u09AE\u09B2\u09BE\u09A8\u09CB \u09AF\u09BE\u09DF\
  \ \u09A4\u09BE\u09B0 \u0989\u09AA\u09B0 \u098F\u09B0 \u09AD\u09BF\u09A4\u09CD\u09A4\
  \u09BF\u0964 \u0986\u09AE\u09B0\u09BE \u098F\u099F\u09BF \u0995\u09B0\u09BF \u0995\
  \u09CD\u09B0\u09CD\u09AF\u09BE\u09B6\u2026"
lastmod: '2024-03-17T18:47:43.772730-06:00'
model: gpt-4-0125-preview
summary: "\u09AD\u09C1\u09B2 \u09B8\u09AE\u09CD\u09AA\u09B0\u09CD\u0995\u09C7 \u09B9\
  \u09BE\u09A4\u09BE\u09B2\u09C7\u09B0 \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2 \u0985\
  \u09A8\u09BE\u0995\u09BE\u0999\u09CD\u0995\u09CD\u09B7\u09BF\u09A4 \u0998\u099F\u09A8\
  \u09BE\u0995\u09C7 \u0986\u09B6\u09BE \u0995\u09B0\u09BE; \u0986\u09AE\u09BE\u09A6\
  \u09C7\u09B0 \u0995\u09CB\u09A1\u09C7 \u09AF\u0996\u09A8 \u0995\u09BF\u099B\u09C1\
  \ \u09AD\u09C1\u09B2 \u0998\u099F\u09C7, \u09A4\u09BE \u0995\u09BF\u09AD\u09BE\u09AC\
  \u09C7 \u09B8\u09BE\u09AE\u09B2\u09BE\u09A8\u09CB \u09AF\u09BE\u09DF \u09A4\u09BE\
  \u09B0 \u0989\u09AA\u09B0 \u098F\u09B0 \u09AD\u09BF\u09A4\u09CD\u09A4\u09BF\u0964\
  \ \u0986\u09AE\u09B0\u09BE \u098F\u099F\u09BF \u0995\u09B0\u09BF \u0995\u09CD\u09B0\
  \u09CD\u09AF\u09BE\u09B6\u2026"
title: "\u09A4\u09CD\u09B0\u09C1\u099F\u09BF\u0997\u09C1\u09B2\u09BF \u09AA\u09B0\u09BF\
  \u099A\u09BE\u09B2\u09A8\u09BE \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি ও কেন?
ভুল সম্পর্কে হাতালের মানে হল অনাকাঙ্ক্ষিত ঘটনাকে আশা করা; আমাদের কোডে যখন কিছু ভুল ঘটে, তা কিভাবে সামলানো যায় তার উপর এর ভিত্তি। আমরা এটি করি ক্র্যাশ এড়াতে এবং ব্যবহারকারীদের একটি মসৃণ অভিজ্ঞতা প্রদান করতে, তখনো যখন অনাকাঙ্ক্ষিত ঘটনা ঘটে।

## কিভাবে:
TypeScript-এ, ভুল সম্পর্কে হাতালের জন্য প্রায়শই `try`, `catch`, এবং `finally` ব্লকগুলি জড়িত হয়।

```typescript
function riskyOperation(): void {
  throw new Error("কিছু ভুল হয়ে গেছে!");
}

function handleErrors() : void {
  try {
    riskyOperation();
  } catch (error) {
    console.error("একটি ভুল ধরা পড়েছে:", error.message);
  } finally {
    console.log("এটি সবসময় চালান হয়, ভুল ঘটুক অথবা না ঘটুক।");
  }
}

handleErrors();
```

নমুনা আউটপুট:

```
একটি ভুল ধরা পড়েছে: কিছু ভুল হয়ে গেছে!
এটি সবসময় চালান হয়, ভুল ঘটুক অথবা না ঘটুক।
```

প্রমিসের সাথে অ্যাসিঙ্ক উদাহরণ:

```typescript
async function asyncRiskyOperation() {
  return new Promise((resolve, reject) => {
    // একটি ভুলকে সিমুলেট করা
    reject("ব্যর্থ রূপে");
  });
}

async function handleAsyncErrors() {
  try {
    await asyncRiskyOperation();
  } catch (error) {
    console.error("একটি অ্যাসিঙ্ক ভুল ধরা পড়েছে:", error);
  }
}

handleAsyncErrors();
```

নমুনা আউটপুট:

```
একটি অ্যাসিঙ্ক ভুল ধরা পড়েছে: ব্যর্থ রূপে
```

## গভীর ডুব
প্রোগ্রামিংয়ের শুরু থেকেই ভুল সম্পর্কে হাতালের মূলনীতি হিসেবে রয়েছে। TypeScript-এ, যা জাভাস্ক্রিপ্টের উপর নির্মিত, ভুল সম্পর্কে হাতালের আরও শক্তিশালী হয়েছে যখন ইসিএমএস্ক্রিপ্ট ২০১৭-এ অ্যাসিঙ্ক/অ্যাওয়েট চালু হয়েছে। এর আগে, আমরা প্রায়শই অ্যাসিঙ্ক্রনাস কোডে ভুল সামলানোর জন্য কলব্যাক ফাংশন এবং প্রমিসগুলিকে নির্ভর করে থাকতাম।

TypeScript-এ `try/catch` এর পরিবর্তে একটি বিকল্প হল ফ্রেমওয়ার্কে যেমন রিয়েক্ট দ্বারা প্রদত্ত ভুল সীমানাগুলি ব্যবহার করা। সার্ভারের পক্ষে, আমরা এক্সপ্রেস.জেএস মতো প্ল্যাটফর্মে মিডওয়্যার ব্যবহার করে ভুল প্রবন্ধনকে কেন্দ্রীভূত করতে পারি।

বাস্তবায়নের দিক থেকে, TypeScript-এর নিজস্ব ভুল সম্পর্কে হাতালের কৌশল নেই বরং এটি জাভাস্ক্রিপ্টের উপর নির্ভর করে। কাস্টম ভুল ক্লাসগুলি `Error` ক্লাসকে এক্সটেন্ড করতে পারে আরও বর্ণনামূলক ভুলের তথ্য প্রদান করতে।

## আরও দেখুন
- [try/catch সম্পর্কে MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/try...catch)
- [Async/Await সম্পর্কে MDN](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Asynchronous/Async_await)
- [রিয়েক্টে ভুল সীমানাগুলি ব্যবহার](https://reactjs.org/docs/error-boundaries.html)
- [এক্সপ্রেস.জেএস ভুল প্রবন্ধন](https://expressjs.com/en/guide/error-handling.html)
