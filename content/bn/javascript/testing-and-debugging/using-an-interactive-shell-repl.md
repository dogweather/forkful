---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:23:48.422177-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Node.js \u098F\u0995\u099F\u09BF\
  \ REPL \u09B8\u09B0\u09AC\u09B0\u09BE\u09B9 \u0995\u09B0\u09C7 \u09AF\u09BE \u099F\
  \u09BE\u09B0\u09CD\u09AE\u09BF\u09A8\u09BE\u09B2\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\
  \u09AF\u09AE\u09C7 \u09AA\u09CD\u09B0\u09BE\u09AA\u09CD\u09AF\u0964 \u098F\u099F\
  \u09BF \u0996\u09C1\u09B2\u09C1\u09A8, \u098F\u09AC\u0982 \u0986\u09AA\u09A8\u09BF\
  \ \u09A4\u09C8\u09B0\u09BF\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u098F\u0995\u099F\
  \u09BF \u09B8\u09CD\u09AC\u09BE\u09A6 \u09B0\u09AF\u09BC\u09C7\u099B\u09C7."
lastmod: '2024-03-17T18:47:44.454904-06:00'
model: gpt-4-0125-preview
summary: "Node.js \u098F\u0995\u099F\u09BF REPL \u09B8\u09B0\u09AC\u09B0\u09BE\u09B9\
  \ \u0995\u09B0\u09C7 \u09AF\u09BE \u099F\u09BE\u09B0\u09CD\u09AE\u09BF\u09A8\u09BE\
  \u09B2\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u09AA\u09CD\u09B0\
  \u09BE\u09AA\u09CD\u09AF\u0964 \u098F\u099F\u09BF \u0996\u09C1\u09B2\u09C1\u09A8\
  , \u098F\u09AC\u0982 \u0986\u09AA\u09A8\u09BF \u09A4\u09C8\u09B0\u09BF\u0964 \u098F\
  \u0996\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09B8\u09CD\u09AC\u09BE\u09A6\
  \ \u09B0\u09AF\u09BC\u09C7\u099B\u09C7."
title: "\u0987\u09A8\u09CD\u099F\u09BE\u09B0\u09AF\u09BC\u09BE\u0995\u09CD\u099F\u09BF\
  \u09AD \u09B6\u09C7\u09B2 (REPL) \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\
  \u09B0\u09BE"
weight: 34
---

## কিভাবে:
Node.js একটি REPL সরবরাহ করে যা টার্মিনালের মাধ্যমে প্রাপ্য। এটি খুলুন, এবং আপনি তৈরি। এখানে একটি স্বাদ রয়েছে:

```javascript
$ node
> let sum = (a, b) => a + b;
অনির্ধারিত
> sum(5, 10);
15
> .exit
```

সোজাসাপটা, তাই না? ভেরিয়েবল, ফাংশন সংজ্ঞায়িত করুন, বা লুপ চালান। কাজ শেষে, `.exit` আপনাকে বাস্তব দুনিয়ায় ফিরিয়ে নিয়ে যায়।

## গভীর দিকে
REPLs ষাটের দশক থেকে চালু আছে - LISP এই ধারণাটির অগ্রদূত। ধারণাটি: প্রোগ্রামারকে তাত্ক্ষণিক প্রতিক্রিয়া দেওয়া। বিকল্প? Node.js REPL ছাড়াও, ব্রাউজার-ভিত্তিক কনসোলের মতো Chrome DevTools, অনলাইন স্যান্ডবক্সের মতো JSFiddle, বা ফুল IDEs যেমন VSCode ইন্টার‌্যাক্টিভ প্লেগ্রাউন্ড সহ রয়েছে।

আড়ালে, REPL ওয়ার্কফ্লোগুলি সাধারণত:
1. ইনপুট পড়া
2. কোড কম্পাইল এবং চালানো
3. আউটপুট প্রিন্ট করা
4. প্রত্যাবর্তন

এটি একটি সাধারণ তবে কার্যকর চক্র যা ইন্টার‌্যাক্টিভ কোডিংয়ে বিশাল প্রভাব ফেলেছে।

## দেখুন আরো
- [Node.js REPL ডকুমেন্টেশন](https://nodejs.org/api/repl.html)
- [REPLs-এ JavaScript মডিউল সম্পর্কে Mozilla-র পরিচয়](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Modules)
- [JSFiddle](https://jsfiddle.net/)
