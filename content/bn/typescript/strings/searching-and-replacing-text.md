---
title:                "টেক্সট অনুসন্ধান এবং প্রতিস্থাপন"
date:                  2024-03-17T18:17:29.821088-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কী এবং কেন?

প্রোগ্রামিংয়ের একটি সাধারণ কাজ হল স্ট্রিংয়ের মধ্যে টেক্সট খুঁজে বের করা এবং প্রতিস্থাপন করা, যা ডাটা প্রক্রিয়া করা এবং ম্যানিপুলেশনে প্রায়শই ব্যবহৃত হয়। কন্টেন্ট পরিমার্জন, ত্রুটি ঠিক করা এবং বৃহদায়তন কোডবেস অথবা ডেটাসেটে অটোমেশন সম্পাদনে এটি অত্যাবশ্যক।

## কিভাবে:

জাভাস্ক্রিপ্টের উপর নির্মিত TypeScript এ স্ট্রিং ম্যানিপুলেশনের জন্য প্রয়োজনীয় পদ্ধতিগুলো প্রদান করে। আমরা বেসিক খুঁজে পাওয়া এবং প্রতিস্থাপনের জন্য `String.prototype.replace()` ব্যবহার করতে পারি। এই স্নিপেটগুলি দেখুন:

```typescript
// সাধারণ স্ট্রিং প্রতিস্থাপন
let text: string = "Hello, World!";
let newText: string = text.replace("World", "TypeScript");
console.log(newText);  // আউটপুট: Hello, TypeScript!

// রেজেক্স সহ গ্লোবাল প্রতিস্থাপন
let regexText: string = "foo bar foo bar";
let globalRegex: RegExp = /foo/g;
let newRegexText: string = regexText.replace(globalRegex, "baz");
console.log(newRegexText);  // আউটপুট: baz bar baz bar

// ফাংশন দিয়ে প্রতিস্থাপন
let dynamicText: string = "I have 2 apples and 5 oranges.";
let fruitCounter: string = dynamicText.replace(/\d+/g, (match) => {
    return (+match * 2).toString();
});
console.log(fruitCounter);  // আউটপুট: I have 4 apples and 10 oranges.
```

## গভীরে যাই

ঐতিহাসিকভাবে, টেক্সট প্রতিস্থাপন এমনকি সর্বপ্রথম টেক্সট-প্রসেসিং টুলগুলিতেও একটি বৈশিষ্ট্য ছিল, `sed` এর মতো ইউনিক্স টুলস আইকনিক উদাহরণ। আধুনিক প্রোগ্রামিংয়ে, আদর্শ মিলের জন্য রেগুলার এক্সপ্রেশন (regex) সহ প্রতিস্থাপন অপারেশনগুলি প্রায়ই শক্তিশালী হয়।

TypeScript এ `String.prototype.replace()`-এর বিকল্প বহু। Lodash এর মতো লাইব্রেরিগুলি অনুরূপ সিনট্যাক্স সহ `_.replace()` অফার করে। আরও উন্নত সিনারিওর জন্য, আপনি নিজের পার্সার বানানো বা সাধারণ স্ট্রিং প্রতিস্থাপনের চেয়ে বেশি রূপান্তর টাস্কের জন্য পার্সার লাইব্রেরিগুলি ব্যবহার করার কথা ভাবতে পারেন।

বাস্তবায়নের কথা বললে, মনে রাখবেন `.replace()` মূল স্ট্রিংটি পরিবর্তন করে না। জাভাস্ক্রিপ্ট এবং TypeScript এ স্ট্রিংগুলি অপরিবর্তনীয়। এই পদ্ধতিটি একটি নতুন স্ট্রিং ফেরত দেয়, সুতরাং যদি আপনি পরিবর্তিত টেক্সট প্রয়োজন হয়, তবে আপনাকে তা সংরক্ষণ করতে হবে, উপরের উদাহরণগুলির মতো।

## দেখুন অবশ্যই

- `replace()` সম্পর্কে MDN ওয়েব ডক্স: [MDN String replace](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- আপনার প্যাটার্ন মিলান দক্ষতা ধারালো করতে রেজেক্স টেস্টিং টুল: [Regex101](https://regex101.com/)
- একটি ভিন্ন পদ্ধতির জন্য Lodash এর স্ট্রিং প্রতিস্থাপন: [Lodash _.replace](https://lodash.com/docs/4.17.15#replace)
