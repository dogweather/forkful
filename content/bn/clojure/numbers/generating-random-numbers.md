---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:51:53.927881-06:00
description: "\u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BF\u0982\u09AF\
  \u09BC\u09C7 \u09B0\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09AE \u09B8\u0982\u0996\
  \u09CD\u09AF\u09BE \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\
  \u09C7 \u09B9\u09B2 \u098F\u09AE\u09A8 \u09AE\u09BE\u09A8 \u09A4\u09C8\u09B0\u09BF\
  \ \u0995\u09B0\u09BE \u09AF\u09BE \u09AA\u09C2\u09B0\u09CD\u09AC\u09C7 \u09AF\u09C1\
  \u0995\u09CD\u09A4\u09BF\u09B8\u0999\u09CD\u0997\u09A4\u09AD\u09BE\u09AC\u09C7 \u09AA\
  \u09C2\u09B0\u09CD\u09AC\u09BE\u09AD\u09BE\u09B8 \u0995\u09B0\u09BE \u09AF\u09BE\
  \u09AF\u09BC \u09A8\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\
  \u09AE\u09BE\u09B0\u09B0\u09BE \u09AC\u09BF\u09AD\u09BF\u09A8\u09CD\u09A8 \u0995\
  \u09BE\u09B0\u09A3\u09C7 \u098F\u099F\u09BF \u0995\u09B0\u09C7\u2026"
lastmod: '2024-03-17T18:47:43.616880-06:00'
model: gpt-4-0125-preview
summary: "\u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BF\u0982\u09AF\
  \u09BC\u09C7 \u09B0\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09AE \u09B8\u0982\u0996\
  \u09CD\u09AF\u09BE \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\
  \u09C7 \u09B9\u09B2 \u098F\u09AE\u09A8 \u09AE\u09BE\u09A8 \u09A4\u09C8\u09B0\u09BF\
  \ \u0995\u09B0\u09BE \u09AF\u09BE \u09AA\u09C2\u09B0\u09CD\u09AC\u09C7 \u09AF\u09C1\
  \u0995\u09CD\u09A4\u09BF\u09B8\u0999\u09CD\u0997\u09A4\u09AD\u09BE\u09AC\u09C7 \u09AA\
  \u09C2\u09B0\u09CD\u09AC\u09BE\u09AD\u09BE\u09B8 \u0995\u09B0\u09BE \u09AF\u09BE\
  \u09AF\u09BC \u09A8\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\
  \u09AE\u09BE\u09B0\u09B0\u09BE \u09AC\u09BF\u09AD\u09BF\u09A8\u09CD\u09A8 \u0995\
  \u09BE\u09B0\u09A3\u09C7 \u098F\u099F\u09BF \u0995\u09B0\u09C7\u2026"
title: "\u098F\u09B2\u09CB\u09AE\u09C7\u09B2\u09CB \u09B8\u0982\u0996\u09CD\u09AF\u09BE\
  \ \u0989\u09CE\u09AA\u09A8\u09CD\u09A8 \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি ও কেন?

প্রোগ্রামিংয়ে র্যান্ডম সংখ্যা তৈরি করা মানে হল এমন মান তৈরি করা যা পূর্বে যুক্তিসঙ্গতভাবে পূর্বাভাস করা যায় না। প্রোগ্রামাররা বিভিন্ন কারণে এটি করে থাকেন, যেমন অনন্য পরিচিতিদাতা তৈরি, গেম ডেভেলপমেন্টে সিনারিও অনুমান, বা বিশ্লেষণের জন্য ডাটা থেকে র্যান্ডম নমুনা নির্বাচন করা।

## কীভাবে:

Clojureতে, র্যান্ডম সংখ্যা তৈরি খুবই সহজ, এবং এজন্য কয়েকটি বিল্ড-ইন ফাংশন রয়েছে যা সরাসরি ব্যবহার করা যেতে পারে।

০ (অন্তর্ভুক্ত) থেকে ১ (অন্তর্ভুক্ত নয়) এর মধ্যে একটি র্যান্ডম ভাসমান-বিন্দু সংখ্যা তৈরি করতে, আপনি `rand` ফাংশন ব্যবহার করতে পারেন:

```Clojure
(rand)
;; উদাহরণ আউটপুট: 0.7094245047062917
```

যদি আপনার একটি নির্দিষ্ট রেঞ্জের মধ্যে একটি পূর্ণসংখ্যা প্রয়োজন হয়, তাহলে `rand-int` ব্যবহার করুন:

```Clojure
(rand-int 10)
;; উদাহরণ আউটপুট: 7
```

এইটা আপনাকে ০ (অন্তর্ভুক্ত) থেকে আপনি যে সংখ্যাটি আলোচনা হিসেবে পাস করেন (অন্তর্ভুক্ত নয়) এর মধ্যে একটি র্যান্ডম পূর্ণসংখ্যা দেয়।

একটি নির্দিষ্ট রেঞ্জের মধ্যে একটি র্যান্ডম সংখ্যা তৈরি করতে (শুধুমাত্র পূর্ণসংখ্যাগুলিতে সীমাবদ্ধ নয়), আপনি `rand`-কে গাণিতিক অপারেশনের সাথে মিশ্রিত করতে পারেন:

```Clojure
(defn rand-range [min max]
  (+ min (* (rand) (- max min))))
;; ব্যবহার
(rand-range 10 20)
;; উদাহরণ আউটপুট: 14.857457734992847
```

এই ফাংশন `rand-range` আপনাকে আপনি যে `min` এবং `max` মান নির্দিষ্ট করেন সেগুলির মধ্যে একটি র্যান্ডম ভাসমান-বিন্দু সংখ্যা ফেরত দেবে।

আরও জটিল বিতরণ বা র্যান্ডম সংখ্যার অনুক্রম প্রয়োজন হলে যেখানে পুনরাবৃত্তি প্রয়োজনিয় (বীজগুলি ব্যবহার করে), আপনাকে হয়তো অতিরিক্ত লাইব্রেরিগুলিতে দেখতে হবে যা বিল্ড-ইন-এর বাইরে গিয়েছে।

## গভীর ডাইভ

অধিকাংশ প্রোগ্রামিং ভাষা, Clojure সহ র্যান্ডম সংখ্যা তৈরির জন্য অন্তর্নিহিত পদ্ধতি সাধারণত একটি প্রিউডো-র্যান্ডম সংখ্যা জেনারেটর (PRNG) এর উপর নির্ভর করে। একটা PRNG একটি অ্যালগোরিদম ব্যবহার করে এমন একটি সংখ্যার অনুক্রম উৎপাদন করে যা র্যান্ডম সংখ্যার বৈশিষ্ট্যগুলি অনুকরণ করে। লক্ষণীয়, যেহেতু এগুলি অ্যালগরিম ভিত্তিক উৎপন্ন, এগুলি সত্যিকারের র্যান্ডম নয় কিন্তু বেশিরভাগ বাস্তবিক উদ্দেশ্য জন্য পর্যাপ্ত হতে পারে।

কম্পিউটিং এর প্রাথমিক দিনগুলিতে, উচ্চ-মানের র্যান্ডম সংখ্যা তৈরি করা একটি গুরুত্বপূর্ণ চ্যালেঞ্জ ছিল, যা র্যান্ডমনেস এবং বিতরণ উন্নত করার জন্য বিভিন্ন অ্যালগোরিদমের উন্নয়নে অনুপ্রেরণা দিয়েছিল। Clojure এর জন্য, এরকম `rand` এবং `rand-int` মতো বিল্ড-ইন ফাংশনগুলি রোজমর্রার ব্যবহারের জন্য সুবিধাজনক এবং সাধারণ ব্যবহারের ক্ষেত্রের ব্যাপক পরিসর আচ্ছাদন করে।

তবে, যে অ্যাপ্লিকেশনগুলির জন্য ক্রিপ্টোগ্রাফিক নিরাপত্তা বা আরও জটিল পরিসংখ্যান স্যাম্পলিং পদ্ধতিগুলির প্রয়োজন, Clojure ডেভেলপাররা প্রায়শই বাইরের লাইব্রেরিগুলিতে যায় যা আরও দৃঢ় এবং বিশেষায়িত PRNGs প্রদান করে। `clj-random` এর মতো লাইব্রেরিগুলি বিভিন্ন অ্যালগোরিদম এবং বীজ নিয়ন্ত্রণের উপর ব্যাপক পরিসরে অ্যাক্সেস প্রদান করে, যা সিমুলেশন, ক্রিপ্টোগ্রাফিক অ্যাপ্লিকেশন, বা যে কোন ডোমেনের জন্য গুরুত্বপূর্ণ হতে পারে যেখানে র্যান্ডম সংখ্যার ক্রমের মান ও পূর্বনির্ধারণীয়তা গুরুত্বপূর্ণ প্রভাব ফেলতে পারে।

Clojure এর র্যান্ডম সংখ্যা তৈরির জন্য বিল্ড-ইন ক্ষমতা অনেক কাজের জন্য যথেষ্ট হলেও, বাইরের লাইব্রেরিগুলি অন্বেষণ করা অধিক অন্তর্দৃষ্টি এবং সাজানো বা আরও গভীর প্রয়োগের বিকল্প সরবরাহ করতে পারে।