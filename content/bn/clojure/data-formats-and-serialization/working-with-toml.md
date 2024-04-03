---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:30:47.646091-06:00
description: "TOML \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\
  \u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u0986\u09AA\u09A8\u09BF \"\u099F\u09AE\u09C7\
  \u09B0 \u0986\u09AC\u09AC\u09CD\u09AF\u0995\u09CD\u09A4, \u09B8\u09B0\u09CD\u09AC\
  \u09A8\u09BF\u09AE\u09CD\u09A8 \u09AD\u09BE\u09B7\u09BE\" \u09AC\u09BE Minimal \"\
  Tom's Obvious, Minimal Language\" \u09AB\u09B0\u09CD\u09AE\u09CD\u09AF\u09BE\u099F\
  \u09C7 \u09A1\u09C7\u099F\u09BE \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C\
  \ \u0995\u09B0\u099B\u09C7\u09A8, \u098F\u099F\u09BF \u098F\u09B0 \u09B8\u09B9\u099C\
  \u2026"
lastmod: '2024-03-17T18:47:43.646969-06:00'
model: gpt-4-0125-preview
summary: "TOML \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\
  \u09BE \u09AE\u09BE\u09A8\u09C7 \u0986\u09AA\u09A8\u09BF \"\u099F\u09AE\u09C7\u09B0\
  \ \u0986\u09AC\u09AC\u09CD\u09AF\u0995\u09CD\u09A4, \u09B8\u09B0\u09CD\u09AC\u09A8\
  \u09BF\u09AE\u09CD\u09A8 \u09AD\u09BE\u09B7\u09BE\" \u09AC\u09BE Minimal \"Tom's\
  \ Obvious, Minimal Language\" \u09AB\u09B0\u09CD\u09AE\u09CD\u09AF\u09BE\u099F\u09C7\
  \ \u09A1\u09C7\u099F\u09BE \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\
  \u09B0\u099B\u09C7\u09A8, \u098F\u099F\u09BF \u098F\u09B0 \u09B8\u09B9\u099C \u09AA\
  \u09BE\u09A0\u09AF\u09CB\u0997\u09CD\u09AF\u09A4\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF\
  \ \u0995\u09A8\u09AB\u09BF\u0997\u09BE\u09B0\u09C7\u09B6\u09A8 \u09AB\u09BE\u0987\
  \u09B2\u0997\u09C1\u09B2\u09BF\u09B0 \u099C\u09A8\u09CD\u09AF \u099C\u09A8\u09AA\
  \u09CD\u09B0\u09BF\u09AF\u09BC\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\
  \u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\
  \u09C7\u0987\u099F\u09AB\u09B0\u09CB\u09AF\u09BC\u09BE\u09B0\u09CD\u09A1 \u0995\u09A8\
  \u09AB\u09BF\u0997\u09C1\u09B0\u09C7\u09B6\u09A8 \u09AE\u09CD\u09AF\u09BE\u09A8\u09C7\
  \u099C\u09AE\u09C7\u09A8\u09CD\u099F\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09AC\
  \u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09C7\
  \u09A8 \u09AF\u09BE \u09AE\u09BE\u09A8\u09AC-\u09AC\u09BE\u09A8\u09CD\u09A7\u09AC\
  \ \u09B8\u09BF\u09A8\u099F\u09CD\u09AF\u09BE\u0995\u09CD\u09B8\u09C7\u09B0 \u09B8\
  \u09BE\u09A5\u09C7 \u09B8\u09BE\u09A5\u09C7\u0987 \u0995\u09BE\u099C \u0995\u09B0\
  \u09C7\u0964."
title: "\u099F\u09AE\u09B2 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\
  \u09B0\u09BE"
weight: 39
---

## কি এবং কেন?
TOML এর সাথে কাজ করা মানে আপনি "টমের আবব্যক্ত, সর্বনিম্ন ভাষা" বা Minimal "Tom's Obvious, Minimal Language" ফর্ম্যাটে ডেটা নিয়ে কাজ করছেন, এটি এর সহজ পাঠযোগ্যতার জন্য কনফিগারেশন ফাইলগুলির জন্য জনপ্রিয়। প্রোগ্রামাররা এটি স্ট্রেইটফরোয়ার্ড কনফিগুরেশন ম্যানেজমেন্টের জন্য ব্যবহার করে থাকেন যা মানব-বান্ধব সিনট্যাক্সের সাথে সাথেই কাজ করে।

## কিভাবে:
Clojure-এ TOML এর সাথে কাজ করতে, আপনার `clj-toml` এর মতো একটি লাইব্রেরির প্রয়োজন। প্রথমে, এটি আপনার `deps.edn`-এ যোগ করুন:

```clojure
{:deps {clj-toml {:mvn/version "0.5.0"}}}
```

এরপর TOML পার্স করুন:

```clojure
(require '[clj-toml.core :as toml])

(def config-str "title = 'TOML Example'")

(def parsed-config (toml/parse-string config-str))

;; পার্স করা TOML থেকে টাইটেল পেতে
(println (:title parsed-config)) ;; আউটপুট: TOML Example
```

TOML জেনারেট করতে:

```clojure
(def data {:title "TOML Example"})

(println (toml/generate-string data))
;; আউটপুট: title = "TOML Example"
```

## গভীর ডাইভ
TOML ২০১৩ সালে টম প্রেস্টন-ওয়ের্নার, GitHub-এর সহ-প্রতিষ্ঠাতা, কনফিগ ফাইলগুলির জন্য YAML এবং JSON এর তুলনায় সহজ বিকল্প হিসেবে তৈরি করেছিলেন। এটি স্পষ্টতা লক্ষ্য করে এবং অতিরিক্ত টুল ছাড়া মানুষ পড়তে পারে এমন একটি বিশেষ উদ্দেশ্যে তৈরি করা হয়েছে। 

JSON প্রায়শই APIs এবং ওয়েব অ্যাপসের জন্য ব্যবহৃত হয়, এবং YAML রেফারেন্স এবং স্ক্রিপ্টের ক্ষমতা সহ জটিল হতে পারে, TOML সাধারণ, টেবিল-ভিত্তিক কাঠামোগুলির উপর ফোকাস দিয়ে বিশেষভাবে অবস্থান নেয়। এই সাধারণতা Rust সম্প্রদায় এবং অন্যান্য আধুনিক ভাষার পরিবেশে বিশেষ জনপ্রিয়।

Clojure, এর সাধারণতা এবং ব্যাবহারিকতায় ফোকাসের দিক থেকে, কনফিগের জন্য TOML এর সাথে ভালো মেলে। `clj-toml` অথবা বিকল্প লাইব্রেরি TOML-এর স্ট্যাটিক ডেটাকে Clojure-এর ডায়নামিক, ফাংশনাল প্রকৃতির মধ্যে অনুবাদ করে।

## দেখে নিন
- TOML-এর GitHub রিপো: [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- `clj-toml` Clojars-এ: [clojars.org/clj-toml](https://clojars.org/clj-toml)
- Clojure ডক্স: [clojure.org](https://clojure.org/guides/getting_started)
- `clj-toml`- এ প্রবেশ: [github.com/lantiga/clj-toml](https://github.com/lantiga/clj-toml)
