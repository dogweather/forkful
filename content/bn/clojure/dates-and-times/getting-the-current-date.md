---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:48:33.427339-06:00
description: "\u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BF\u0982 \u098F\
  \ \u09B8\u09BE\u09AE\u09CD\u09AA\u09CD\u09B0\u09A4\u09BF\u0995 \u09A4\u09BE\u09B0\
  \u09BF\u0996 \u09AA\u09BE\u0993\u09AF\u09BC\u09BE \u0985\u09A4\u09CD\u09AF\u09A8\
  \u09CD\u09A4 \u0997\u09C1\u09B0\u09C1\u09A4\u09CD\u09AC\u09AA\u09C2\u09B0\u09CD\u09A3\
  , \u09AF\u09C7\u0997\u09C1\u09B2\u09CB\u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7 \u09B2\
  \u0997\u09BF\u0982, \u0998\u099F\u09A8\u09BE\u09AC\u09B2\u09C0\u09B0 \u099F\u09BE\
  \u0987\u09AE\u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09AE\u09CD\u09AA\u09BF\u0982,\
  \ \u098F\u09AC\u0982 \u0995\u09BE\u09B0\u09CD\u09AF\u0995\u09CD\u09B0\u09AE \u09B6\
  \u09BF\u09A1\u09BF\u0989\u09B2\u09BF\u0982 \u09B0\u09AF\u09BC\u09C7\u099B\u09C7\u0964\
  \ Clojure \u098F, \u09AF\u09BE \u09B9\u09B2\u2026"
lastmod: '2024-03-17T18:47:43.632308-06:00'
model: gpt-4-0125-preview
summary: "\u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BF\u0982 \u098F\
  \ \u09B8\u09BE\u09AE\u09CD\u09AA\u09CD\u09B0\u09A4\u09BF\u0995 \u09A4\u09BE\u09B0\
  \u09BF\u0996 \u09AA\u09BE\u0993\u09AF\u09BC\u09BE \u0985\u09A4\u09CD\u09AF\u09A8\
  \u09CD\u09A4 \u0997\u09C1\u09B0\u09C1\u09A4\u09CD\u09AC\u09AA\u09C2\u09B0\u09CD\u09A3\
  , \u09AF\u09C7\u0997\u09C1\u09B2\u09CB\u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7 \u09B2\
  \u0997\u09BF\u0982, \u0998\u099F\u09A8\u09BE\u09AC\u09B2\u09C0\u09B0 \u099F\u09BE\
  \u0987\u09AE\u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09AE\u09CD\u09AA\u09BF\u0982,\
  \ \u098F\u09AC\u0982 \u0995\u09BE\u09B0\u09CD\u09AF\u0995\u09CD\u09B0\u09AE \u09B6\
  \u09BF\u09A1\u09BF\u0989\u09B2\u09BF\u0982 \u09B0\u09AF\u09BC\u09C7\u099B\u09C7\u0964\
  \ Clojure \u098F, \u09AF\u09BE \u09B9\u09B2\u2026"
title: "\u09AC\u09B0\u09CD\u09A4\u09AE\u09BE\u09A8 \u09A4\u09BE\u09B0\u09BF\u0996\
  \ \u09AA\u09C7\u09A4\u09C7"
---

{{< edit_this_page >}}

## কি এবং কেন?
প্রোগ্রামিং এ সাম্প্রতিক তারিখ পাওয়া অত্যন্ত গুরুত্বপূর্ণ, যেগুলোর মধ্যে লগিং, ঘটনাবলীর টাইমস্ট্যাম্পিং, এবং কার্যক্রম শিডিউলিং রয়েছে। Clojure এ, যা হল জেভিএম এর উপর একটি Lisp ডায়ালেক্ট, এই কাজটি জাভা ইন্টারঅপ ক্ষমতা ব্যবহার করে, যা জাভা ডেট-টাইম API এর সমৃদ্ধ অ্যাক্সেস প্রদান করে সহজ উপায়ে।

## কিভাবে:

### জাভা ইন্টারঅপ ব্যবহার করে
Clojure এর জাভার সাথে নির্বিঘ্ন ইন্টারপেরিবিলিটি আপনাকে সরাসরি জাভা ডেট-টাইম API এ ঢুকতে দেয়। এখানে আপনি কিভাবে সাম্প্রতিক তারিখ পেতে পারেন:

```clojure
(import java.time.LocalDate)

(defn get-current-date []
  (str (LocalDate/now)))

;; নমুনা আউটপুট
(get-current-date) ; "2023-04-15"
```

### clj-time লাইব্রেরি ব্যবহার করে
একটি আরও আদি Clojure সমাধানের জন্য, আপনি `clj-time` লাইব্রেরি বেছে নিতে পারেন, যা হল Joda-Time এর একটি র‍্যাপার, তবে অধিকাংশ নতুন প্রকল্পের জন্য, বিল্ট-ইন জাভা 8 ডেট-টাইম API সুপারিশ করা হয়। তবুও, যদি আপনি `clj-time` প্রেফার করেন অথবা প্রয়োজনীয় মনে করেন:

প্রথমে, `clj-time` আপনার প্রজেক্টের ডিপেন্ডেন্সিজে যোগ করুন। আপনার `project.clj` তে অন্তর্ভুক্ত করুন:

```clojure
[clj-time "0.15.2"]
```

তারপর, এটি ব্যবহার করে সাম্প্রতিক তারিখ নিন:

```clojure
(require '[clj-time.core :as time])

(defn get-current-date-clj-time []
  (str (time/now)))

;; নমুনা আউটপুট
(get-current-date-clj-time) ; "2023-04-15T12:34:56.789Z"
```

উভয় পদ্ধতিই Clojure তে সাম্প্রতিক তারিখ পেতে দ্রুত, কার্যকরী উপায় প্রদান করে, অধস্তন জাভা প্ল্যাটফর্মের শক্তি বা একটি Clojure-বিশেষ লাইব্রেরির সুবিধা কাজে লাগানো হয়।
