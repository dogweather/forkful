---
changelog:
- 2024-01-28, dogweather, reviewed and added links
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:51:57.620153-06:00
description: "\u0995\u09CB\u09A1\u0995\u09C7 \u09AB\u09BE\u0982\u09B6\u09A8\u09C7\u09B0\
  \ \u09AE\u09A7\u09CD\u09AF\u09C7 \u09B8\u09BE\u099C\u09BE\u09A8\u09CB\u09B0 \u09AC\
  \u09BF\u09B7\u09AF\u09BC\u099F\u09BF \u09B9'\u09B2 \u09A8\u09BF\u09B0\u09CD\u09A6\
  \u09BF\u09B7\u09CD\u099F \u0995\u09BE\u099C \u09B8\u09AE\u09CD\u09AA\u09BE\u09A6\
  \u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09B8\u09CD\u0995\u09CD\u09B0\u09BF\
  \u09AA\u09CD\u099F\u09C7\u09B0 \u0996\u09A3\u09CD\u09A1\u0997\u09C1\u09B2\u09BF\
  \ \u098F\u0995\u09A4\u09CD\u09B0\u09BF\u09A4 \u0995\u09B0\u09BE\u0964 \u0986\u09AE\
  \u09B0\u09BE \u098F\u099F\u09BF \u0995\u09B0\u09BF \u0995\u09BE\u09B0\u09A3 \u098F\
  \u099F\u09BF \u0995\u09CB\u09A1\u0995\u09C7 \u09AA\u09A1\u09BC\u09A4\u09C7, \u09AA\
  \u09B0\u09C0\u0995\u09CD\u09B7\u09BE \u0995\u09B0\u09A4\u09C7,\u2026"
lastmod: '2024-03-17T18:47:44.503549-06:00'
model: gpt-4-0125-preview
summary: "\u0995\u09CB\u09A1\u0995\u09C7 \u09AB\u09BE\u0982\u09B6\u09A8\u09C7\u09B0\
  \ \u09AE\u09A7\u09CD\u09AF\u09C7 \u09B8\u09BE\u099C\u09BE\u09A8\u09CB\u09B0 \u09AC\
  \u09BF\u09B7\u09AF\u09BC\u099F\u09BF \u09B9'\u09B2 \u09A8\u09BF\u09B0\u09CD\u09A6\
  \u09BF\u09B7\u09CD\u099F \u0995\u09BE\u099C \u09B8\u09AE\u09CD\u09AA\u09BE\u09A6\
  \u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09B8\u09CD\u0995\u09CD\u09B0\u09BF\
  \u09AA\u09CD\u099F\u09C7\u09B0 \u0996\u09A3\u09CD\u09A1\u0997\u09C1\u09B2\u09BF\
  \ \u098F\u0995\u09A4\u09CD\u09B0\u09BF\u09A4 \u0995\u09B0\u09BE\u0964 \u0986\u09AE\
  \u09B0\u09BE \u098F\u099F\u09BF \u0995\u09B0\u09BF \u0995\u09BE\u09B0\u09A3 \u098F\
  \u099F\u09BF \u0995\u09CB\u09A1\u0995\u09C7 \u09AA\u09A1\u09BC\u09A4\u09C7, \u09AA\
  \u09B0\u09C0\u0995\u09CD\u09B7\u09BE \u0995\u09B0\u09A4\u09C7,\u2026"
title: "\u0995\u09CB\u09A1\u0995\u09C7 \u09AB\u09BE\u0982\u09B6\u09A8\u0997\u09C1\u09B2\
  \u09BF\u09A4\u09C7 \u0986\u09AF\u09BC\u09CB\u099C\u09A8 \u0995\u09B0\u09BE"
weight: 18
---

## কি এবং কেন?
কোডকে ফাংশনের মধ্যে সাজানোর বিষয়টি হ'ল নির্দিষ্ট কাজ সম্পাদনের জন্য স্ক্রিপ্টের খণ্ডগুলি একত্রিত করা। আমরা এটি করি কারণ এটি কোডকে পড়তে, পরীক্ষা করতে, এবং পুনঃব্যবহার করতে সহজ করে দেয় — কেউই কোড স্প্যাঘেটির দলদলের মধ্য দিয়ে চলতে চায় না।

## কিভাবে:
ফিশে, আপনি `function` কীওয়ার্ডের সাথে একটি ফাংশন লিখুন, এতে একটি নাম দিন, এবং `end` দিয়ে শেষ করুন। এখানে একটি সাধারণ উদাহরণ দেওয়া হল:

```fish
function hello
    echo "Hello, World!"
end

hello
```

আউটপুট:
```
Hello, World!
```

এখন, চলুন একটি ব্যবহারকারীকে অভিবাদন জানান:

```fish
function greet
    set user (whoami)
    echo "Hey there, $user!"
end

greet
```

আউটপুট:
```
Hey there, your_username!
```

এটি সেশনগুলির মধ্যে সংরক্ষণ করতে, `funcsave greet` ব্যবহার করুন।

## গভীর ডুব
ফিশ শেল ফাংশনগুলি মিনি-স্ক্রিপ্টের মতো - আপনি প্রায় সবকিছুই এতে ঠেলে দিতে পারেন। ঐতিহাসিকভাবে, শেল স্ক্রিপ্টিংয়ে ফাংশনের ধারণাটি পুনরাবৃত্তিমূলক টাইপিং এবং ডিবাগিংয়ে অগণিত ঘন্টা বাঁচিয়েছে। পাইথনের মতো প্রোগ্রামিং ভাষাগুলিতে ফাংশনের মতো, শেল ফাংশনগুলি গঠনের চেয়ে সুবিধার বিষয়ে বেশি।

কিছু শেল, যেমন বাশ, `function` বা সোজা ব্রেসিস ব্যবহার করে। ফিশ স্পষ্ট এবং পঠনযোগ্য `function ... end` এ থাকে। ফিশ ফাংশনের মধ্যে, আপনি পরামিতি, লোকাল ভ্যারিয়েবলের জন্য `set -l`, এবং এমনকি অন্য ফাংশনের মধ্যে একটি ফাংশন সংজ্ঞায়িত করতে পারেন।

আপনার `return` মানের প্রয়োজন হবে না কারণ ফিশ এতে বড় নয়; আপনার ফাংশনের আউটপুটই এর রিটার্ন। এবং আপনি যদি ভবিষ্যত সেশনের জন্য স্থায়ী ফাংশনগুলি চান, `funcsave` মনে রাখুন।

## আরও দেখুন:

- ফাংশনে ফিশ টিউটোরিয়াল: [https://fishshell.com/docs/current/tutorial.html#tut_functions](https://fishshell.com/docs/current/tutorial.html#functions)

### ফাংশন কমান্ডস

- [function](https://fishshell.com/docs/current/cmds/function.html) — একটি ফাংশন তৈরি করুন
- [functions](https://fishshell.com/docs/current/cmds/functions.html) — ফাংশনগুলি প্রিন্ট করুন অথবা মুছে ফেলুন
- [funcsave](https://fishshell.com/docs/current/cmds/funcsave.html) — একটি ফাংশনের সংজ্ঞা ব্যবহারকারীর অটোলোড ডিরেক্টরিতে সংরক্ষণ করুন
- [funced](https://fishshell.com/docs/current/cmds/funced.html) — একটি ফাংশনকে ইন্টারাক্টিভভাবে সম্পাদনা করুন
