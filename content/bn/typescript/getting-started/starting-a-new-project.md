---
title:                "নতুন প্রকল্প শুরু করা"
date:                  2024-03-17T18:21:40.801462-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?
TypeScript এ একটি নতুন প্রকল্প শুরু করা মানে এর ওপর কোড করার জন্য একটি মজবুত ভিত তৈরি করা। প্রোগ্রামাররা নতুন প্রকল্প শুরু করেন নতুন ধারণাগুলি কাজে লাগানোর, ধারণাগুলি পরীক্ষা করার অথবা নতুন জিনিস শিখার জন্য।

## কীভাবে:
```TypeScript
// প্রথম ধাপ: টাইপস্ক্রিপ্ট বৈশ্বিকভাবে ইন্সটল করুন (যদি ইন্সটল না থাকে)
npm install -g typescript

// দ্বিতীয় ধাপ: আপনার প্রকল্পের জন্য একটি নতুন ডিরেক্টরি তৈরি করুন
mkdir my-new-project
cd my-new-project

// তৃতীয় ধাপ: একটি নতুন নোড প্রকল্প শুরু করুন
npm init -y

// চতুর্থ ধাপ: আপনার প্রকল্পে টাইপস্ক্রিপ্ট ইন্সটল করুন
npm install typescript --save-dev

// পঞ্চম ধাপ: টাইপস্ক্রিপ্ট প্রকল্পটি শুরু করে tsconfig.json তৈরি করুন
tsc --init

// নমুনা tsconfig.json আউটপুট (সংক্ষিপ্ততার জন্য কিছু ক্ষেত্র বাদ দেওয়া হয়েছে)
{
  "compilerOptions": {
    "target": "es5",
    "module": "commonjs",
    "strict": true,
    ...
  }
}

// ষষ্ঠ ধাপ: একটি সহজ 'hello.ts' টাইপস্ক্রিপ্ট ফাইল তৈরি করুন
echo 'console.log("Hello, TypeScript!");' > hello.ts

// সপ্তম ধাপ: টাইপস্ক্রিপ্ট ফাইলটি কম্পাইল করুন এবং চালান
tsc hello.ts
node hello.js

// নমুনা আউটপুট
Hello, TypeScript!
```

## গভীর ডুব
TypeScript, যা হল JavaScript এর একটি সুপারসেট, মাইক্রোসফট দ্বারা উন্নীত এবং অক্টোবর ২০১২ সালে প্রথম মুক্তিপ্রাপ্ত। এটি JavaScript এ স্থির ধরণের সংযোজন করে, যা রানটাইমের আগে ত্রুটিগুলি ধরা এবং কোড নেভিগেশন এবং রিফ্যাক্টরিং এর মতো IDE ফিচার সমর্থন করতে সাহায্য করতে পারে।

উপরের প্রক্রিয়াটি npm (নোড প্যাকেজ ম্যানেজার) ব্যবহার করলেও, TypeScript প্রকল্পগুলি পরিচালনা করার অন্যান্য উপায় রয়েছে, যেমন Yarn বা pnpm রয়েছে। TypeScript প্রকল্প শুরু করার বিকল্পগুলির মধ্যে একটি স্টার্টার কিট ব্যবহার করে একটি প্রকল্প তৈরি করা বা GitHub এর মতো রেপোজিটরিগুলি থেকে একটি ব্রুটপ্যাচ ক্লোন করা অন্তর্ভুক্ত।

`tsconfig.json` গুরুত্বপূর্ণ; এটি নির্দেশ করে কীভাবে TypeScript Compiler (tsc) আপনার TypeScript কোডকে JavaScript এ পরিবর্তন করে। কম্পাইলার অপশনগুলি tweaking আপনাকে ভিন্ন ইসিএমএস্ক্রিপ্ট সংস্করণগুলি, মডিউল সিস্টেম এবং আরও অনেক কিছু লক্ষ্য করা, আপনার প্রকল্পের প্রয়োজনীয়তাগুলিতে tailor করা সহজ করে দেয়।

## আরও দেখুন
- TypeScript অফিশিয়াল ডক্স: [https://www.typescriptlang.org/docs/](https://www.typescriptlang.org/docs/)
- TypeScript GitHub রেপো: [https://github.com/microsoft/TypeScript](https://github.com/microsoft/TypeScript)
- TypeScript গভীর ডুব: [https://basarat.gitbook.io/typescript/](https://basarat.gitbook.io/typescript/)
- অসাধারণ TypeScript: [https://github.com/dzharii/awesome-typescript](https://github.com/dzharii/awesome-typescript)
