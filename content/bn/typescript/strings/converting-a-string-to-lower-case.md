---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:43.110018-06:00
description: "\u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\
  \u09C7 \u09B2\u09CB\u09AF\u09BC\u09BE\u09B0\u0995\u09C7\u09B8\u09C7 \u09AA\u09B0\
  \u09BF\u09AC\u09B0\u09CD\u09A4\u09A8 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7\
  \ \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u09AF\u09BC\u09C7\u09B0 \u09AA\u09CD\
  \u09B0\u09A4\u09BF\u099F\u09BF \u0985\u0995\u09CD\u09B7\u09B0\u0995\u09C7 \u099B\
  \u09CB\u099F \u09B9\u09BE\u09A4\u09C7\u09B0 \u0985\u0995\u09CD\u09B7\u09B0\u09C7\
  \ \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8 \u0995\u09B0\u09BE\u0964 \u09AA\
  \u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\
  \u09BF \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3\u09A4 \u09B8\u09C1\u09B8\u0982\u0997\
  \u09A4\u09BF\u09B0 \u099C\u09A8\u09CD\u09AF \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\
  \u09C7\u09A8,\u2026"
lastmod: '2024-03-17T18:47:43.751100-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\
  \u09C7 \u09B2\u09CB\u09AF\u09BC\u09BE\u09B0\u0995\u09C7\u09B8\u09C7 \u09AA\u09B0\
  \u09BF\u09AC\u09B0\u09CD\u09A4\u09A8 \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7\
  \ \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u09AF\u09BC\u09C7\u09B0 \u09AA\u09CD\
  \u09B0\u09A4\u09BF\u099F\u09BF \u0985\u0995\u09CD\u09B7\u09B0\u0995\u09C7 \u099B\
  \u09CB\u099F \u09B9\u09BE\u09A4\u09C7\u09B0 \u0985\u0995\u09CD\u09B7\u09B0\u09C7\
  \ \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8 \u0995\u09B0\u09BE\u0964 \u09AA\
  \u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\
  \u09BF \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3\u09A4 \u09B8\u09C1\u09B8\u0982\u0997\
  \u09A4\u09BF\u09B0 \u099C\u09A8\u09CD\u09AF \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\
  \u09C7\u09A8,\u2026"
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\u09C7 \u09B2\u09CB\u09AF\u09BC\
  \u09BE\u09B0 \u0995\u09C7\u09B8\u09C7 \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\
  \u09B0 \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?
একটি স্ট্রিংকে লোয়ারকেসে পরিবর্তন করা মানে স্ট্রিংয়ের প্রতিটি অক্ষরকে ছোট হাতের অক্ষরে পরিবর্তন করা। প্রোগ্রামাররা এটি সাধারণত সুসংগতির জন্য করে থাকেন, বিশেষ করে কেস-ইনসেনসিটিভ তুলনার জন্য, যেমন কমান্ডের তালিকা বা সংরক্ষিত ডেটা বিরুদ্ধে ব্যবহারকারীর ইনপুট যাচাই করার সময়।

## কিভাবে:
TypeScript-এ, একটি স্ট্রিংকে লোয়ারকেসে পরিণত করা খুবই সহজ। শুধু আপনার স্ট্রিং-এ `.toLowerCase()` ফাংশনটি ডাকুন। এই রকম ভাবে:

```typescript
let myString: string = "HeLLo, WorLD!";
let lowerCaseString: string = myString.toLowerCase();
console.log(lowerCaseString); // আউটপুট: "hello, world!"
```

সহজ, তাই না?

## গভীর ডুব
গত দিনে, টেক্সট প্রসেসিং সবসময় সুসংগত হতো না, এবং অক্ষরের এনকোডিং একটি বন্য পশ্চিম হতে পারে। এখন, ইউনিকোড এবং মানকৃত পদ্ধতি সাহায্যে, ভাষাগুলি জুড়ে কেসগুলো ঐক্যবদ্ধ হয়। `.toLowerCase()`-এর তুলনায়, একটি পুরোনো-স্কুলের পদ্ধতি (যেমন ASCII ম্যানিপুলেশন) হল প্রস্তর যুগের। বিকল্পগুলি (যেমন `.toLocaleLowerCase()`) সঠিক কেসিংয়ের জন্য লোকেল-নির্দিষ্ট নিয়মগুলি বিবেচনা করে, যা হাতের কাছে হতে পারে। অভ্যন্তরীণভাবে,  JavaScript (এবং TypeScript এক্সটেনশন হিসেবে) -এ `.toLowerCase()` প্রতিটি অক্ষরের মধ্যে যায় এবং, যদি এটি একটি আপারকেস অক্ষর হয়, ইউনিকোড ম্যাপিংস ভিত্তিক নিম্ন-কেস সমতুল্যে পরিণত করে।

## আরও দেখুন
আপনার টেক্সট-প্রসেসিং গেমকে মশলাদার করতে ও আরও স্ট্রিং জিমন্যাস্টিক্সের জন্য, এগুলো দেখে নিন:

- `.toLowerCase()` উপর MDN ডকুমেন্টেশন: [MDN toLowerCase](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- TypeScript অফিসিয়াল ডকস: [TypeScriptlang.org](https://www.typescriptlang.org/docs/)
- লোকেল-নির্দিষ্ট রূপান্তরকরণের বিষয়ে ভালোভাবে বুঝতে: [MDN toLocaleLowerCase](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLocaleLowerCase)
- ইউনিকোড মানদণ্ডের গভীর জ্ঞানের জন্য: [Unicode Case Mapping](https://www.unicode.org/reports/tr21/tr21-5.html)
