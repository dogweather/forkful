---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:23:50.909620-06:00
description: "REPL, \u0985\u09B0\u09CD\u09A5\u09BE\u09CE Read-Eval-Print Loop, \u098F\
  \u0995\u099F\u09BF \u0987\u09A8\u09CD\u099F\u09BE\u09B0\u09C7\u0995\u09CD\u099F\u09BF\
  \u09AD \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BF\u0982 \u09AA\
  \u09B0\u09BF\u09AC\u09C7\u09B6, \u09AF\u09C7\u099F\u09BF \u098F\u0995\u0995 \u09AC\
  \u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u0995\u09BE\u09B0\u09C0\u09B0 \u0987\u09A8\u09AA\
  \u09C1\u099F \u09A8\u09BF\u09DF\u09C7, \u09A4\u09BE \u09A8\u09BF\u09B0\u09CD\u09AC\
  \u09BE\u09B9 \u0995\u09B0\u09C7, \u098F\u09AC\u0982 \u09AB\u09B2\u09BE\u09AB\u09B2\
  \ \u09AB\u09BF\u09B0\u09BF\u09DF\u09C7 \u09A6\u09C7\u09DF\u0964\u2026"
lastmod: '2024-03-17T18:47:44.499465-06:00'
model: gpt-4-0125-preview
summary: "REPL, \u0985\u09B0\u09CD\u09A5\u09BE\u09CE Read-Eval-Print Loop, \u098F\u0995\
  \u099F\u09BF \u0987\u09A8\u09CD\u099F\u09BE\u09B0\u09C7\u0995\u09CD\u099F\u09BF\u09AD\
  \ \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BF\u0982 \u09AA\u09B0\
  \u09BF\u09AC\u09C7\u09B6, \u09AF\u09C7\u099F\u09BF \u098F\u0995\u0995 \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09BE\u09B0\u0995\u09BE\u09B0\u09C0\u09B0 \u0987\u09A8\u09AA\u09C1\
  \u099F \u09A8\u09BF\u09DF\u09C7, \u09A4\u09BE \u09A8\u09BF\u09B0\u09CD\u09AC\u09BE\
  \u09B9 \u0995\u09B0\u09C7, \u098F\u09AC\u0982 \u09AB\u09B2\u09BE\u09AB\u09B2 \u09AB\
  \u09BF\u09B0\u09BF\u09DF\u09C7 \u09A6\u09C7\u09DF\u0964\u2026"
title: "\u0987\u09A8\u09CD\u099F\u09BE\u09B0\u09AF\u09BC\u09BE\u0995\u09CD\u099F\u09BF\
  \u09AD \u09B6\u09C7\u09B2 (REPL) \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\
  \u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?
REPL, অর্থাৎ Read-Eval-Print Loop, একটি ইন্টারেক্টিভ প্রোগ্রামিং পরিবেশ, যেটি একক ব্যবহারকারীর ইনপুট নিয়ে, তা নির্বাহ করে, এবং ফলাফল ফিরিয়ে দেয়। প্রোগ্রামাররা তাত্ক্ষণিক ফিডব্যাক, ডিবাগিং, এবং কোডিং ধারণাগুলি নিয়ে দ্রুত গতিতে পরীক্ষা করার জন্য এটি ব্যবহার করেন, যা একটি সম্পূর্ণ প্রোগ্রাম কম্পাইল এবং রান করার অতিরিক্ত ব্যয়কে এড়িয়ে যায়।

## কিভাবে:
Fish এ, ইন্টারেক্টিভ শেলটি হল ডিফল্ট মোড যখন আপনি এটি চালু করেন। এখানে এটি কর্মে কেমন দেখাচ্ছে তার একটি উদাহরণ:

```Fish Shell
> set color blue
> echo "The sky is $color"
The sky is blue
```

আপনি নিজের তৈরি ফাংশন এবং কমান্ড সাবস্টিটিউশন নিয়েও খেলতে পারেন:

```Fish Shell
> function cheer
      echo "Go Fish $argv!"
  end
> cheer Coders
Go Fish Coders!
```

শুধু ফাংশন সংজ্ঞায়িত করা নয়, আপনি মুহূর্তে কোড স্নিপেট নির্বাহ করতে এবং তাত্ক্ষণিক আউটপুট দেখতে পারেন:

```Fish Shell
> math "40 / 2"
20
```

## গভীর ডাইভ
REPLs এর ধারণা মূলত 1960 এর দশকে Lisp প্রোগ্রামিং ভাষায় প্রচলিত। ইন্টারেক্টিভ প্রোগ্রামিং এর এই রূপ Python এর `ipython` এবং Ruby এর `irb` এর মতো পরিবেশগুলির জন্য মানদণ্ড স্থির করে। Fish ব্যবহারকারীদের জন্য বান্ধবী এবং ইন্টারেক্টিভ ব্যবহারের দিক থেকে ট্রেন্ড অনুসরণ করে চলেছে।

Fish অন্যান্য শেলের মত বাশ থেকে ভিন্ন, এটি সর্বাগ্রে ইন্টারেক্টিভিটি মাথায় রেখে ডিজাইন করা হয়েছে। এটি সিনট্যাক্স হাইলাইটিং, অটোসাজেশন, এবং ট্যাব কমপ্লিশন প্রদান করে যা একটি REPL-শৈলীর কাজে শক্তিশালী করে তোলে। আরও ভাল, আপনার কমান্ডগুলি স্মরণ করা হয় এবং সন্ধানযোগ্য হয়, যা পুনরাবৃত্ত পরীক্ষাকে অত্যন্ত সহজ করে তোলে।

Fish এর REPL এর বিকল্প হতে পারে `bash` বা `zsh`, যখন `bash-completion` বা `oh-my-zsh` এর মতো এক্সটেনশনের সাথে জুড়ে দেওয়া হয়, কিন্তু Fish সাধারণত একটি সমৃদ্ধ রেডি-টু-ইউজ অভিজ্ঞতা প্রদান করে।

## দেখুনও:
- Fish ডকুমেন্টেশন: https://fishshell.com/docs/current/index.html
- Fish এবং অন্যান্য শেলের তুলনামূলক একটি মজাদার আলোচনা: https://www.slant.co/versus/2209/3686/~fish_vs_bash
- REPLs এর গভীর ডাইভ: https://en.wikipedia.org/wiki/Read–eval–print_loop
- Lisp এর ইন্টারেক্টিভ প্রোগ্রামিং, একটি ঐতিহাসিক দৃষ্টিকোণ: http://www.paulgraham.com/ilisp.html
