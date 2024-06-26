---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:18:52.462202-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Clojure \u098F, \u0986\u09AA\u09A8\
  \u09BF \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3\u09A4 HTTP \u0985\u09A8\u09C1\u09B0\u09CB\
  \u09A7\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF `clj-http` \u09B2\u09BE\u0987\u09AC\u09CD\
  \u09B0\u09C7\u09B0\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09AC\u09C7\u09A8, \u09AF\u09BE\u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7 \u09AE\u09CC\
  \u09B2\u09BF\u0995 \u09AA\u09CD\u09B0\u09AE\u09BE\u09A3\u09C0\u0995\u09B0\u09A3\
  \ \u0985\u09A8\u09CD\u09A4\u09B0\u09CD\u09AD\u09C1\u0995\u09CD\u09A4\u0964 \u0986\
  \u09B8\u09C1\u09A8 \u0986\u09AA\u09A8\u09BE\u09B0 `project.clj` \u098F\u2026"
lastmod: '2024-04-05T21:53:51.665103-06:00'
model: gpt-4-0125-preview
summary: "Clojure \u098F, \u0986\u09AA\u09A8\u09BF \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3\
  \u09A4 HTTP \u0985\u09A8\u09C1\u09B0\u09CB\u09A7\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF\
  \ `clj-http` \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09AC\u09C7\u09A8, \u09AF\u09BE\u09B0\
  \ \u09AE\u09A7\u09CD\u09AF\u09C7 \u09AE\u09CC\u09B2\u09BF\u0995 \u09AA\u09CD\u09B0\
  \u09AE\u09BE\u09A3\u09C0\u0995\u09B0\u09A3 \u0985\u09A8\u09CD\u09A4\u09B0\u09CD\u09AD\
  \u09C1\u0995\u09CD\u09A4\u0964 \u0986\u09B8\u09C1\u09A8 \u0986\u09AA\u09A8\u09BE\
  \u09B0 `project.clj` \u098F \u09A8\u09BF\u09B0\u09CD\u09AD\u09B0\u09A4\u09BE (`[clj-http\
  \ \"3.12.3\"]` \u0986\u09AE\u09BE\u09B0 \u09B6\u09C7\u09B7 \u0986\u09AA\u09A1\u09C7\
  \u099F \u0985\u09A8\u09C1\u09B8\u09BE\u09B0\u09C7) \u09AF\u09CB\u0997 \u0995\u09B0\
  \u09BE\u09B0 \u09B8\u09BE\u09A5\u09C7 \u09B6\u09C1\u09B0\u09C1 \u0995\u09B0\u09BF\
  \u0964 \u098F\u09B0\u09AA\u09B0, \u098F\u0996\u09BE\u09A8\u09C7 \u0995\u09BF\u09AD\
  \u09BE\u09AC\u09C7 \u09AE\u09CC\u09B2\u09BF\u0995 \u09AA\u09CD\u09B0\u09AE\u09BE\
  \u09A3\u09C0\u0995\u09B0\u09A3\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u098F\u0995\
  \u099F\u09BF GET \u0985\u09A8\u09C1\u09B0\u09CB\u09A7 \u09A4\u09C8\u09B0\u09BF \u0995\
  \u09B0\u09AC\u09C7\u09A8."
title: "\u09AC\u09C7\u09B8\u09BF\u0995 \u0985\u09A5\u09C7\u09A8\u09CD\u099F\u09BF\u0995\
  \u09C7\u09B6\u09A8 \u09B8\u09B9 HTTP \u09B0\u09BF\u0995\u09C1\u09DF\u09C7\u09B8\u09CD\
  \u099F \u09AA\u09CD\u09B0\u09C7\u09B0\u09A3"
weight: 45
---

## কিভাবে:
Clojure এ, আপনি সাধারণত HTTP অনুরোধের জন্য `clj-http` লাইব্রেরি ব্যবহার করবেন, যার মধ্যে মৌলিক প্রমাণীকরণ অন্তর্ভুক্ত। আসুন আপনার `project.clj` এ নির্ভরতা (`[clj-http "3.12.3"]` আমার শেষ আপডেট অনুসারে) যোগ করার সাথে শুরু করি।

এরপর, এখানে কিভাবে মৌলিক প্রমাণীকরণের সাথে একটি GET অনুরোধ তৈরি করবেন:

```clojure
(require '[clj-http.client :as client])

(let [response (client/get "https://your-api.com/resource"
                           {:basic-auth ["username" "password"]})]
  (println "স্থিতি:" (:status response))
  (println "বডি:" (:body response)))
```
`"https://your-api.com/resource"`, `"username"`, এবং `"password"` এর স্থলে আপনার বিবরণ প্রতিস্থাপন করুন। এই কোড একটি GET অনুরোধ পাঠায় এবং উত্তরের স্থিতি এবং বডি প্রিন্ট করে।

নমুনা আউটপুট এরকম দেখাবে:

```
স্থিতি: 200
বডি: {JSON ডেটা বা এখানে কিছু অন্যান্য}
```

## গভীর ডুব
HTTP মৌলিক প্রমাণীকরণ ইন্টারনেটের প্রাথমিক প্রটোকলে মূল পাওয়া যায়। এটি বেস64 ব্যবহার করে এনকোড করা একটি HTTP হেডারে ব্যবহারকারীর নাম ও পাসওয়ার্ড পাস করে। এটি সহজ, কিন্তু সবচেয়ে নিরাপদ নয় কারণ যদি আপত্তিকর ভাবে গ্রাহক ধরা পড়ে যায় তবে প্রমাণগুলি সহজেই ডিকোড করা যেতে পারে।

বিকল্প:
- **ডাইজেস্ট প্রমাণীকরণ**: আরো জটিল, পাসওয়ার্ডের একটি হ্যাশ করা সংস্করণ পাঠানো জড়িত।
- **OAuth**: একটি আরো দৃঢ় অনুমোদন সিস্টেম যা ব্যবহারকারীর নাম এবং পাসওয়ার্ড পাঠানোর প্রয়োজন নেই।
- **API কী**: ঐতিহ্যবাহী লগইন প্রমাণীকরণের পরিবর্তে ব্যবহৃত অনন্য টোকেন।

`clj-http` এর অন্তরালে, বিকল্প হ্যাশম্যাপে `:basic-auth` নির্দিষ্ট করা লাইব্রেরিকে আপনার প্রমাণীকরণগুলি এনকোড করে এবং তাদেরকে HTTP `Authorization` হেডারে যুক্ত করতে উদ্বুদ্ধ করে। সার্ভারটি যখন অনুরোধ পায়, এটি হেডারটি ডিকোড করে এবং প্রমাণীকরণগুলি পরীক্ষা করে।

মাথায় রাখবেন যে নিরাপদ সংক্রমণের জন্য, আপনার প্রমাণগুলি অন্যদের দ্বারা আটকে যাওয়া থেকে রক্ষা করতে HTTPS ব্যবহার করা উচিত।

## আরো দেখুন
- clj-http GitHub রিপো: https://github.com/dakrone/clj-http
- Clojure অফিসিয়াল ডকুমেন্টেশন: https://clojure.org/
- MDN এ HTTP প্রমাণীকরণ: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication 
- OAuth বুঝতে: https://oauth.net/
