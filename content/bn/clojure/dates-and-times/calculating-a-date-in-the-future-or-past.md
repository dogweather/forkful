---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:38.148102-06:00
description: "\u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4 \u09AC\u09BE \u0985\u09A4\
  \u09C0\u09A4\u09C7\u09B0 \u09A4\u09BE\u09B0\u09BF\u0996 \u0997\u09A3\u09A8\u09BE\
  \ \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u09A4\u09BE\u09B0\u09BF\u0996\u0997\
  \u09C1\u09B2\u09BF \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\u09B0\
  \u09BE \u09AF\u09BE\u09A4\u09C7 \u099C\u09BE\u09A8\u09BE \u09AF\u09BE\u09AF\u09BC\
  \ \u098F\u0995 \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09B8\u09AE\
  \u09AF\u09BC\u09C7\u09B0 \u09AA\u09B0\u09C7 \u0995\u09BF \u09B9\u09AC\u09C7 \u09AC\
  \u09BE \u0995\u09BF \u099B\u09BF\u09B2\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\
  \u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u0987\u09AD\u09C7\
  \u09A8\u09CD\u099F \u09B6\u09BF\u09A1\u09BF\u0989\u09B2\u09BF\u0982,\u2026"
lastmod: '2024-03-17T18:47:43.635631-06:00'
model: gpt-4-0125-preview
summary: "\u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4 \u09AC\u09BE \u0985\u09A4\u09C0\
  \u09A4\u09C7\u09B0 \u09A4\u09BE\u09B0\u09BF\u0996 \u0997\u09A3\u09A8\u09BE \u0995\
  \u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u09A4\u09BE\u09B0\u09BF\u0996\u0997\u09C1\
  \u09B2\u09BF \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE\
  \ \u09AF\u09BE\u09A4\u09C7 \u099C\u09BE\u09A8\u09BE \u09AF\u09BE\u09AF\u09BC \u098F\
  \u0995 \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09B8\u09AE\u09AF\
  \u09BC\u09C7\u09B0 \u09AA\u09B0\u09C7 \u0995\u09BF \u09B9\u09AC\u09C7 \u09AC\u09BE\
  \ \u0995\u09BF \u099B\u09BF\u09B2\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\
  \u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u0987\u09AD\u09C7\u09A8\
  \u09CD\u099F \u09B6\u09BF\u09A1\u09BF\u0989\u09B2\u09BF\u0982,\u2026"
title: "\u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4 \u09AC\u09BE \u0985\u09A4\u09C0\
  \u09A4\u09C7\u09B0 \u09A4\u09BE\u09B0\u09BF\u0996 \u0997\u09A3\u09A8\u09BE \u0995\
  \u09B0\u09BE"
---

{{< edit_this_page >}}

## কি ও কেন?

ভবিষ্যত বা অতীতের তারিখ গণনা করা মানে তারিখগুলি নিয়ে কাজ করা যাতে জানা যায় এক নির্দিষ্ট সময়ের পরে কি হবে বা কি ছিল। প্রোগ্রামাররা এটি ইভেন্ট শিডিউলিং, রিমাইন্ডার সেট করা, অথবা মেয়াদ শেষের দিন বের করার মতো কাজের জন্য করে থাকেন।

## কিভাবে:

Clojure এ, আপনি মূলত তারিখ অপারেশানের জন্য `clj-time` লাইব্রেরি ব্যবহার করবেন। এখানে একটি দ্রুত ডেমো দেওয়া হল:

```clojure
(require '[clj-time.core :as time])
(require '[clj-time.coerce :as coerce])
(require '[clj-time.periodic :as periodic])

;; বর্তমান তারিখে ৫ দিন যোগ করা 
(let [now (time/now)
      five-days (time/plus now (time/days 5))]
  (str "এখন থেকে পাঁচ দিন: " (coerce/to-string five-days)))

;; একটি নির্দিষ্ট তারিখ থেকে ১০ দিন বাদ দেওয়া
(let [specific-date (coerce/to-date-time "2023-03-01T12:00:00.000Z")
      ten-days-ago (time/minus specific-date (time/days 10))]
  (str "১ মার্চ, ২০২৩ এর আগের দশ দিন: " (coerce/to-string ten-days-ago)))
```

নমুনা আউটপুট:
```
"এখন থেকে পাঁচ দিন: ২০২৩-০৩-২৩T০৮:০০:০০.০০০Z"
"১ মার্চ, ২০২৩ এর আগের দশ দিন: ২০২৩-০২-১৯T১২:০০:০০.০০০Z"
```

## গভীরে যাওয়া:

প্রারম্ভিক দিনগুলিতে, কোডাররা জাভার `Date` এবং `Calendar` ক্লাসগুলি ব্যবহার করতেন। কিন্তু, আসুন সৎ হই, তারা একটি বিরক্তি - বাচাল ও ভুলের প্রবণ। `clj-time` লাইব্রেরি কিছুটা হলেও সাহস যুগিয়েছে, যেটা Joda-Time's ডেভেলপার-ফ্রেন্ডলি API কে ঘিরে রেখেছে।

কোনো বিকল্প? জাভা ৮ তে `java.time` (JSR-310) চালু করা হয়, যা বেশ ভালো, কিন্তু Clojure's এর বিশ্বে, আমরা এখনো `clj-time` এর সাথে আরামদায়ক।

তারিখ গণনা করার সময়, আপনি "দিন" এবং "মাস" এর মতো ধারণাগুলির জন্য পিরিয়ড এবং নির্দিষ্ট মিলিসেকেন্ড গণনার জন্য সময়কাল ব্যবহার করেন। মনে রাখবেন সময় অঞ্চল - তারিখ এবং সময় সময় অঞ্চলের নিয়ম অনুসারে নাটকীয়ভাবে পরিবর্তন হতে পারে, এবং ডেলাইট সেভিং টাইম (DST) আপনার কাজে বাধা দিতে পারে।

## আরও দেখুন

- `clj-time` GitHub রেপো: [https://github.com/clj-time/clj-time](https://github.com/clj-time/clj-time)
- Clojure's `java-time`: [https://github.com/dm3/clojure.java-time](https://github.com/dm3/clojure.java-time)
