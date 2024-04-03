---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:47:08.684036-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09AB\u09BF\u09B6 \u09B6\u09C7\
  \u09B2\u09C7, \u0986\u09AA\u09A8\u09BF `mktemp` \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09C7 \u098F\u0995\u099F\u09BF \u0985\u09B8\u09CD\u09A5\u09BE\
  \u09AF\u09BC\u09C0 \u09AB\u09BE\u0987\u09B2 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\
  \u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\u0964 \u098F\u0996\u09BE\u09A8\u09C7\
  \ \u098F\u0995\u099F\u09BF \u09A6\u09CD\u09B0\u09C1\u09A4 \u0989\u09A6\u09BE\u09B9\
  \u09B0\u09A3 \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2."
lastmod: '2024-03-17T18:47:44.517745-06:00'
model: gpt-4-0125-preview
summary: "\u09AB\u09BF\u09B6 \u09B6\u09C7\u09B2\u09C7, \u0986\u09AA\u09A8\u09BF `mktemp`\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u098F\u0995\u099F\
  \u09BF \u0985\u09B8\u09CD\u09A5\u09BE\u09AF\u09BC\u09C0 \u09AB\u09BE\u0987\u09B2\
  \ \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\
  \u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09A6\u09CD\u09B0\
  \u09C1\u09A4 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3 \u09A6\u09C7\u0993\u09AF\u09BC\
  \u09BE \u09B9\u09B2."
title: "\u098F\u0995\u099F\u09BF \u0985\u09B8\u09CD\u09A5\u09BE\u09AF\u09BC\u09C0\
  \ \u09AB\u09BE\u0987\u09B2 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE"
weight: 21
---

## কিভাবে:
ফিশ শেলে, আপনি `mktemp` ব্যবহার করে একটি অস্থায়ী ফাইল তৈরি করতে পারেন। এখানে একটি দ্রুত উদাহরণ দেওয়া হল:

```fish
set tempfile (mktemp)
echo "হ্যালো, অস্থায়ী পৃথিবী!" > $tempfile
cat $tempfile
rm $tempfile
```

এবং আপনি এমন কিছু দেখতে পাবেন:

```shell
হ্যালো, অস্থায়ী পৃথিবী!
```

এটি একটি অস্থায়ী ফাইল তৈরি করে, এতে একটি সারি লেখে, সামগ্রী দেখায়, এবং তারপর ফাইলটি মুছে ফেলে।

## গভীর ডুব
এক সময়, অস্থায়ী ফাইলগুলি প্রায়ই ম্যানুয়ালি তৈরি করা হত, যা সম্ভাব্য নাম দ্বন্দ্ব এবং নিরাপত্তা সমস্যার জন্ম দিতে পারে। `mktemp` এই সমস্যার সমাধান! এই কমান্ডটি একটি অনন্য নামের সাথে একটি ফাইল তৈরি করে, ফাইল সংঘর্ষের ঝুঁকি হ্রাস করে।

বিকল্প পদ্ধতি অন্তর্ভুক্ত লিনাক্সে `/dev/shm` এ লেখা অথবা মেমরি-ভিত্তিক ফাইল সিস্টেম ব্যবহার করা। তবে, এই পদ্ধতিগুলি `mktemp` এর মত পোর্টেবল নয়।

অস্থায়ী ফাইলের জীবনকালের জন্য, এটি মনে রাখা জরুরি যে তারা উত্পাদনকারী প্রোগ্রাম দ্বারা মোছা উচিত। এটি নিশ্চিত করে যে কোনও বাকি ফাইল সিস্টেম স্পেস ব্যবহার করে না। কিছু সিস্টেমে, `/tmp` ডিরেক্টরি পুনরায় বুটের সময় পরিষ্কার হয়ে যায়, তবে আপনার উচিত নয় এই আচারণের উপর পরিচ্ছন্নতার জন্য নির্ভর করা।

## আরও দেখুন
- ফিশ শেল ডকুমেন্টেশন: [https://fishshell.com/docs/current/](https://fishshell.com/docs/current/)
- `mktemp` ম্যানুয়াল: [https://www.gnu.org/software/autogen/mktemp.html](https://www.gnu.org/software/autogen/mktemp.html)
- ফাইলসিস্টেম হাইয়ারার্কি স্ট্যান্ডার্ড: [https://refspecs.linuxfoundation.org/FHS_3.0/fhs/index.html](https://refspecs.linuxfoundation.org/FHS_3.0/fhs/index.html)
