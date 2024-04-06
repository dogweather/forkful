---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:19:51.577502-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09A8\u09AE\u09C1\u09A8\u09BE\
  \ \u0986\u0989\u099F\u09AA\u09C1\u099F."
lastmod: '2024-04-05T21:53:51.907832-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u09AC\u09C7\u09B8\u09BF\u0995 \u0985\u09A5\u09C7\u09A8\u09CD\u099F\u09BF\u0995\
  \u09C7\u09B6\u09A8 \u09B8\u09B9 HTTP \u09B0\u09BF\u0995\u09C1\u09DF\u09C7\u09B8\u09CD\
  \u099F \u09AA\u09CD\u09B0\u09C7\u09B0\u09A3"
weight: 45
---

## কিভাবে:
```typescript
import axios from 'axios';

// আপনার ইউজারনেম এবং পাসওয়ার্ড এনকোড করুন
const token = Buffer.from('yourUsername:yourPassword').toString('base64');
const url = 'https://your.api/endpoint';

// Axios দিয়ে HTTP অনুরোধ সেটআপ করুন
axios.get(url, {
  headers: {
    'Authorization': `Basic ${token}`
  }
})
.then(response => {
  console.log(response.data); // এটি আপনার প্রত্যাশিত আউটপুট
})
.catch(error => {
  console.error("Oops, কিছু ভুল হয়েছে!", error);
});
```

নমুনা আউটপুট:

```
{ "message": "তুমি ভেতরে! গোপন API ল্যান্ডে স্বাগতম।" }
```

## গভীরে ডুব:
একসময় OAuth এবং JWTs প্রচলন আগে, basic auth ছিল প্রাথমিক সমাধান। এটি আজও অভ্যন্তরীণ টুল বা প্রুফ অফ কনসেপ্টস (PoCs) এর জন্য কার্যকর। ধারণাটি সরল: 'Authorization' সহ একটি হেডারে জুড়ে দিন, 'Basic ' + একটি base64 এনকোডেড 'username:password' ব্যবহার করে। দেখুন না, আপনি গেটের ভিতরে।

কিন্তু সব সুন্দর্য নয়। ঝুঁকি আছে - আপনি যদি HTTPS ব্যবহার না করেন, আপনি আপনার ক্রিডেনশিয়ালগুলি চিৎকার করে বলে ফেলছেন। বিকল্পগুলি? OAuth2 টোকেন, JWTs, API কী - এগুলি শক্তিশালী, নীরব প্রকারের মত। এগুলো অনুরূপ উদ্দেশ্য পরিবেশন করে কিন্তু আরও জটিলতা এবং নিরাপত্তা সহ।

TypeScript-এ basic auth বাস্তবায়ন করার ক্ষেত্রে, 'axios' বা 'fetch' সাধারণ পছন্দ। আমাদের ক্ষেত্রে, `axios` কাস্টম হেডার সেট করা সহজ করে তোলে। প্লাস, এটি প্রমাইস ফেরত দেয়, যা `async/await`-এর সাথে একটি স্বপ্নের মত কাজ করে।

মনে রাখবেন: 'Basic' শীঘ্রই আধুনিক ওয়েবে এর বয়স প্রকাশ পাবে যেখানে HTTPS অপরিহার্য এবং নিরাপত্তা মানদণ্ড উচ্চতর। তবে, অভ্যন্তরীণ নেটওয়ার্কে অথবা যেখানে উচ্চ নিরাপত্তা অত্যাবশ্যক নয়, এটি সহজ।

## আরও দেখুন
আরও অথেনটিকেশন পদ্ধতি এবং নিরাপত্তা সেরা প্রচলনের জন্য:

- [MDN ওয়েব ডকস: Authorization](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Authorization)
- [OWASP অথেনটিকেশন চিট শিট](https://cheatsheetseries.owasp.org/cheatsheets/Authentication_Cheat_Sheet.html)
- কাস্টম HTTP হেডারের জন্য 'axios' অফিশিয়াল ডকুমেন্টেশন: [Axios ডকস](https://axios-http.com/docs/req_config)
