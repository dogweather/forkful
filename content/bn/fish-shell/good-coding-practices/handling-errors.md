---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:51:05.214726-06:00
description: "\u09A4\u09CD\u09B0\u09C1\u099F\u09BF \u09AA\u09B0\u09BF\u099A\u09BE\u09B2\
  \u09A8\u09BE \u0986\u09AA\u09A8\u09BE\u09B0 \u09B8\u09CD\u0995\u09CD\u09B0\u09BF\
  \u09AA\u09CD\u099F\u0995\u09C7 \u0985\u09AA\u09CD\u09B0\u09A4\u09CD\u09AF\u09BE\u09B6\
  \u09BF\u09A4 \u09AA\u09B0\u09BF\u09B8\u09CD\u09A5\u09BF\u09A4\u09BF \u09AE\u09CB\
  \u0995\u09BE\u09AC\u09C7\u09B2\u09BE \u0995\u09B0\u09A4\u09C7 \u09B8\u09BE\u09B9\
  \u09BE\u09AF\u09CD\u09AF \u0995\u09B0\u09C7\u0964 \u0986\u09AE\u09B0\u09BE \u098F\
  \u099F\u09BF \u0995\u09B0\u09BF \u09AC\u09CD\u09AF\u09B0\u09CD\u09A5\u09A4\u09BE\
  \ \u09AA\u09B0\u09BF\u099A\u09BE\u09B2\u09A8\u09BE \u0995\u09B0\u09BE\u09B0 \u099C\
  \u09A8\u09CD\u09AF, \u09AF\u09BE\u09A4\u09C7 \u0986\u09AE\u09BE\u09A6\u09C7\u09B0\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u0995\u09BE\u09B0\u09C0\u09A6\u09C7\
  \u09B0\u2026"
lastmod: '2024-03-17T18:47:44.505913-06:00'
model: gpt-4-0125-preview
summary: "\u09A4\u09CD\u09B0\u09C1\u099F\u09BF \u09AA\u09B0\u09BF\u099A\u09BE\u09B2\
  \u09A8\u09BE \u0986\u09AA\u09A8\u09BE\u09B0 \u09B8\u09CD\u0995\u09CD\u09B0\u09BF\
  \u09AA\u09CD\u099F\u0995\u09C7 \u0985\u09AA\u09CD\u09B0\u09A4\u09CD\u09AF\u09BE\u09B6\
  \u09BF\u09A4 \u09AA\u09B0\u09BF\u09B8\u09CD\u09A5\u09BF\u09A4\u09BF \u09AE\u09CB\
  \u0995\u09BE\u09AC\u09C7\u09B2\u09BE \u0995\u09B0\u09A4\u09C7 \u09B8\u09BE\u09B9\
  \u09BE\u09AF\u09CD\u09AF \u0995\u09B0\u09C7\u0964 \u0986\u09AE\u09B0\u09BE \u098F\
  \u099F\u09BF \u0995\u09B0\u09BF \u09AC\u09CD\u09AF\u09B0\u09CD\u09A5\u09A4\u09BE\
  \ \u09AA\u09B0\u09BF\u099A\u09BE\u09B2\u09A8\u09BE \u0995\u09B0\u09BE\u09B0 \u099C\
  \u09A8\u09CD\u09AF, \u09AF\u09BE\u09A4\u09C7 \u0986\u09AE\u09BE\u09A6\u09C7\u09B0\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u0995\u09BE\u09B0\u09C0\u09A6\u09C7\
  \u09B0\u2026"
title: "\u09A4\u09CD\u09B0\u09C1\u099F\u09BF\u0997\u09C1\u09B2\u09BF \u09AA\u09B0\u09BF\
  \u099A\u09BE\u09B2\u09A8\u09BE \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?
ত্রুটি পরিচালনা আপনার স্ক্রিপ্টকে অপ্রত্যাশিত পরিস্থিতি মোকাবেলা করতে সাহায্য করে। আমরা এটি করি ব্যর্থতা পরিচালনা করার জন্য, যাতে আমাদের ব্যবহারকারীদের চুল পাকা না হয়।

## কিভাবে:
Fish এ ত্রুটি ধরতে, `status` কমান্ড এবং শর্তাধীন বিবরণের ওপর নির্ভর করুন। ধরা যাক `ping` ব্যর্থ হয়েছে; এখানে তা কিভাবে সনাক্ত করবেন:

```fish
ping -c 1 example.com
if not status is-success
    echo "Something fishy happened with the ping."
end
```

`ping` ব্যর্থ হলে উদাহরণ আউটপুট:

```
Something fishy happened with the ping.
```

একটি নির্দিষ্ট ত্রুটি কোড পরিচালনা করার জন্য, `status --is` ব্যবহার করুন:

```fish
false
if status --is 1
    echo "Caught an error with code 1."
end
```

উদাহরণ আউটপুট:
```
Caught an error with code 1.
```

একটি আরো দৃঢ় পদ্ধতির জন্য, একটি ফাংশন ব্যবহার বিবেচনা করুন:

```fish
function try_ping
    ping -c 1 example.com
    or begin
        echo "Ping failed with status $status"
        return 1
    end
end

try_ping
```

## বিস্তারিত বিবেচনা
Fish এ ত্রুটি পরিচালনা উচ্চস্তরের ভাষাগুলি থেকে পরিচিত হওয়া `try/catch` প্যারাডাইমের সাথে মেলে না। পরিবর্তে, আপনি `status` কমান্ড দ্বারা প্রদান করা সরাসরি প্রস্থান স্ট্যাটাস পান।

ঐতিহাসিকভাবে, Unix-এর মতো সিস্টেমগুলিতে, একটি প্রস্থান স্ট্যাটাস `0` মানে সফলতা, যেকোনো অ-শূন্য মান ত্রুটির ইঙ্গিত দেয়, যা সাধারণত বিভিন্ন ব্যর্থতার কারণ প্রতিফলিত করে। এই রীতি বেশিরভাগ কমান্ড লাইন ইউটিলিটিগুলিতে এবং তাই, Fish নিজেই ব্যবহার করে থাকে।

Fish এ `status` পরীক্ষার বিকল্পগুলি অন্যান্য শেলগুলিতে `trap` এর মাধ্যমে সিগনাল পরিচালনা করা অন্তর্ভুক্ত, কিন্তু Fish সরাসরি স্ট্যাটাস পরীক্ষা পছন্দ করে, কারণ এটি পরিষ্কার এবং পার্শ্বপ্রভাবে কম প্রবণ।

বাস্তবায়নের দিক থেকে, Fish এ ত্রুটি পরিচালনা এর সহজ তবে শক্তিশালী প্রকৃতি বজায় রাখে, এর অ-ব্লকিং প্রকৃতি এবং স্পষ্ট সিনট্যাক্সের উপর জোর দেওয়ার কারণে, যেমন উদাহরণগুলিতে দেখানো হয়েছে। ত্রুটি কোডগুলি ফাংশনের সাথে সুন্দরভাবে মিশে যায়, যা মডিউলার এবং পাঠযোগ্য ত্রুটি পরিচালনা সম্ভব করে তোলে।

## দেখুন
- Fish ডকুমেন্টেশন শর্তাধীন বিবরণের উপর: https://fishshell.com/docs/current/language.html#conditionals
- Fish টিউটোরিয়াল ত্রুটি পরিচালনায়: https://fishshell.com/docs/current/tutorial.html#error-handling
