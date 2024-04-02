---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:42:43.583604-06:00
description: "\u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\
  \u09A1 \u098F\u09B0\u09B0 (stderr) \u098F \u09B2\u09C7\u0996\u09BE\u09B0 \u09AC\u09BF\
  \u09B7\u09AF\u09BC\u099F\u09BF \u09AD\u09C1\u09B2 \u09AC\u09BE\u09B0\u09CD\u09A4\
  \u09BE \u098F\u09AC\u0982 \u09A8\u09BF\u09B0\u09CD\u09A3\u09AF\u09BC \u09AC\u09BE\
  \u09B0\u09CD\u09A4\u09BE\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u09B8\u09CD\u099F\u09CD\
  \u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1 \u0986\u0989\u099F\u09AA\u09C1\
  \u099F (stdout) \u09A5\u09C7\u0995\u09C7 \u0986\u09B2\u09BE\u09A6\u09BE \u0995\u09B0\
  \u09C7 stderr \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u09AE\u09C7 \u09AA\u09BE\u09A0\
  \u09BE\u09A8\u09CB\u09B0 \u09B8\u09BE\u09A5\u09C7\u2026"
lastmod: '2024-03-17T18:47:43.639304-06:00'
model: gpt-4-0125-preview
summary: "\u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\
  \u09A1 \u098F\u09B0\u09B0 (stderr) \u098F \u09B2\u09C7\u0996\u09BE\u09B0 \u09AC\u09BF\
  \u09B7\u09AF\u09BC\u099F\u09BF \u09AD\u09C1\u09B2 \u09AC\u09BE\u09B0\u09CD\u09A4\
  \u09BE \u098F\u09AC\u0982 \u09A8\u09BF\u09B0\u09CD\u09A3\u09AF\u09BC \u09AC\u09BE\
  \u09B0\u09CD\u09A4\u09BE\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u09B8\u09CD\u099F\u09CD\
  \u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1 \u0986\u0989\u099F\u09AA\u09C1\
  \u099F (stdout) \u09A5\u09C7\u0995\u09C7 \u0986\u09B2\u09BE\u09A6\u09BE \u0995\u09B0\
  \u09C7 stderr \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u09AE\u09C7 \u09AA\u09BE\u09A0\
  \u09BE\u09A8\u09CB\u09B0 \u09B8\u09BE\u09A5\u09C7\u2026"
title: "\u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1\
  \ \u098F\u09B0\u09B0\u09C7 \u09B2\u09BF\u0996\u09A8"
weight: 25
---

## কি এবং কেন?
স্ট্যান্ডার্ড এরর (stderr) এ লেখার বিষয়টি ভুল বার্তা এবং নির্ণয় বার্তাগুলিকে স্ট্যান্ডার্ড আউটপুট (stdout) থেকে আলাদা করে stderr স্ট্রিমে পাঠানোর সাথে জড়িত। প্রোগ্রামাররা এটি করেন সাধারণ প্রোগ্রাম আউটপুট এবং ভুল বার্তাগুলি পৃথক করে আরও কার্যকর ডিবাগিং এবং লগিং সম্ভব করতে।

## কিভাবে:
ক্লোজারে, আপনি `*err*` স্ট্রিম ব্যবহার করে stderr এ লিখতে পারেন। এখানে একটি মৌলিক উদাহরণ দেওয়া হল:

```clojure
(.write *err* "এটি একটি ভুল বার্তা।\n")
```

মনে রাখবেন যে একটি বার্তা লেখার পরে, আপনার স্ট্রিমটি ফ্লাশ করা উচিত যাতে বার্তাটি তাৎক্ষণিকভাবে আউটপুট হয়:

```clojure
(flush)
```

stderr এ নমুনা আউটপুট:
```
এটি একটি ভুল বার্তা।
```

যদি আপনি এক্সেপশন হ্যান্ডল করছেন, আপনি stderr এ স্ট্যাক ট্রেসের মুদ্রণ করতে চাইতে পারেন। এর জন্য `printStackTrace` ব্যবহার করুন:

```clojure
(try
  ;; কোড যা এক্সেপশন তৈরি করতে পারে
  (/ 1 0)
  (catch Exception e
    (.printStackTrace e *err*)))
```

আরও গঠিত ভুল লগিং জন্য, `timbre` এর মতো তৃতীয়-পক্ষের লাইব্রেরিগুলি stderr এ লগ করতে কনফিগার করা যেতে পারে। এখানে একটি মৌলিক সেটআপ এবং ব্যবহারের উদাহরণ দেওয়া হল:

প্রথমে, `timbre` কে আপনার নির্ভরতায় যোগ করুন। তারপর এটিকে stderr ব্যবহার করে কনফিগার করুন:

```clojure
(require '[taoensso.timbre :as timbre])

(timbre/set-config! [:appenders :standard-out :enabled?] false) ;; stdout লগিং নিষ্ক্রিয় করুন
(timbre/set-config! [:appenders :spit :enabled?] false) ;; ফাইল লগিং নিষ্ক্রিয় করুন
(timbre/set-config! [:appenders :stderr :min-level] :error) ;; ভুলের জন্য stderr সক্রিয় করুন 

(timbre/error "আপনার অনুরোধ প্রক্রিয়া করার সময় একটি ভুল ঘটেছে।")
```

এটি ভুল-স্তরের বার্তাগুলিকে stderr এ পরিচালনা করবে, এগুলিকে স্ট্যান্ডার্ড অ্যাপ্লিকেশন আউটপুট থেকে পৃথক করে।
