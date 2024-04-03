---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:55.517405-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Fish Shell \u098F, \u0986\u09AE\
  \u09B0\u09BE `date` \u0995\u09AE\u09BE\u09A8\u09CD\u09A1 \u09AC\u09CD\u09AF\u09AC\
  \u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09A6\u09C1\u099F\u09BF \u09A4\u09BE\u09B0\
  \u09BF\u0996\u09C7\u09B0 \u09A4\u09C1\u09B2\u09A8\u09BE \u0995\u09B0\u09A4\u09C7\
  \ \u09AA\u09BE\u09B0\u09BF\u0964 \u09A8\u09C0\u099A\u09C7 \u0989\u09A6\u09BE\u09B9\
  \u09B0\u09A3 \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2\u0964."
lastmod: '2024-03-17T18:47:44.510995-06:00'
model: gpt-4-0125-preview
summary: "Fish Shell \u098F, \u0986\u09AE\u09B0\u09BE `date` \u0995\u09AE\u09BE\u09A8\
  \u09CD\u09A1 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09A6\
  \u09C1\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996\u09C7\u09B0 \u09A4\u09C1\u09B2\
  \u09A8\u09BE \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09BF\u0964 \u09A8\u09C0\
  \u099A\u09C7 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3 \u09A6\u09C7\u0993\u09AF\u09BC\
  \u09BE \u09B9\u09B2\u0964."
title: "\u09A6\u09C1\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996 \u09A4\u09C1\u09B2\
  \u09A8\u09BE \u0995\u09B0\u09BE"
weight: 27
---

## কিভাবে:
Fish Shell এ, আমরা `date` কমান্ড ব্যবহার করে দুটি তারিখের তুলনা করতে পারি। নীচে উদাহরণ দেওয়া হল।

```fish
# এপক থেকে সেকেন্ডে বর্তমান তারিখ পাওয়া
set current_date (date +%s)

# নির্দিষ্ট তারিখকে এপক থেকে সেকেন্ডে রূপান্তর
set specific_date (date -d "2023-04-01" +%s)

# তারিখগুলির তুলনা
if test $specific_date -lt $current_date
    echo "নির্দিষ্ট তারিখ বর্তমান তারিখের আগে।"
else if test $specific_date -eq $current_date
    echo "তারিখগুলি একই।"
else
    echo "নির্দিষ্ট তারিখ বর্তমান তারিখের পরে।"
end
```
যদি বর্তমান তারিখ ২০২৩ সালের ১লা এপ্রিল পরে হয়:
```
নির্দিষ্ট তারিখ বর্তমান তারিখের আগে।
```

## গভীর ডাইভ
ঐতিহাসিকভাবে, প্রোগ্রামিংয়ে তারিখের তুলনা বিভিন্ন তারিখের ফর্ম্যাট এবং টাইম জোনের জটিলতার কারণে একটু কঠিন ছিল। Fish Shell এর অন্তর্নির্মিত `date` ফাংশন এই কাজটি সহজ করে তোলে, তারিখগুলিকে Unix epoch (১৯৭০ সালের ১লা জানুয়ারি) থেকে সেকেন্ডে রূপান্তর করে। এটি আমাদের একটি সার্বজনীন সময়ে তুলনা করার একটি বিন্দু দেয়।

Fish Shell এর বিকল্পগুলি তারিখের তুলনা করার জন্য পাইথনের মতো স্ক্রিপ্টিং ভাষা বা Unix-ভিত্তিক সিস্টেমগুলিতে পাওয়া যায় `date` ম্যানিপুলেশন টুলস অন্তর্ভুক্ত করে, যেমন GNU core utilities (coreutils) -এ `dateutil`। বাস্তবায়নের দিক থেকে, যখন আমরা `date +%s` ব্যবহার করি, Fish অভ্যন্তরীণভাবে সিস্টেম `date` কমান্ডটি ডাকে, যা কেন এটি ক্রস-প্ল্যাটফর্মে এত কার্যকর।

তারিখের তুলনা ক্রোন জবগুলি, ব্যাকআপ স্ক্রিপ্ট, এবং সময়-ভিত্তিক অ্যাক্সেস নিয়ন্ত্রণের জন্যও অপরিহার্য। তারিখের তুলনার প্রতি স্বাচ্ছন্দ্য মানে সহজ অটোমেশন এবং কম সময়-সংক্রান্ত বাগের প্রত্যাশা।

## আরো দেখুন
- [Fish Shell ডকুমেন্টেশন](https://fishshell.com/docs/current/index.html)
- [GNU Coreutils: Date](https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html)
- [দ্য ইউনিক্স এপক টাইম](https://en.wikipedia.org/wiki/Unix_time)
