---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:30:47.646091-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Clojure-\u098F TOML \u098F\u09B0\
  \ \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09A4\u09C7, \u0986\u09AA\
  \u09A8\u09BE\u09B0 `clj-toml` \u098F\u09B0 \u09AE\u09A4\u09CB \u098F\u0995\u099F\
  \u09BF \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u09B0 \u09AA\u09CD\
  \u09B0\u09AF\u09BC\u09CB\u099C\u09A8\u0964 \u09AA\u09CD\u09B0\u09A5\u09AE\u09C7\
  , \u098F\u099F\u09BF \u0986\u09AA\u09A8\u09BE\u09B0 `deps.edn`-\u098F \u09AF\u09CB\
  \u0997 \u0995\u09B0\u09C1\u09A8."
lastmod: '2024-03-17T18:47:43.646969-06:00'
model: gpt-4-0125-preview
summary: "Clojure-\u098F TOML \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C\
  \ \u0995\u09B0\u09A4\u09C7, \u0986\u09AA\u09A8\u09BE\u09B0 `clj-toml` \u098F\u09B0\
  \ \u09AE\u09A4\u09CB \u098F\u0995\u099F\u09BF \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\
  \u09C7\u09B0\u09BF\u09B0 \u09AA\u09CD\u09B0\u09AF\u09BC\u09CB\u099C\u09A8\u0964\
  \ \u09AA\u09CD\u09B0\u09A5\u09AE\u09C7, \u098F\u099F\u09BF \u0986\u09AA\u09A8\u09BE\
  \u09B0 `deps.edn`-\u098F \u09AF\u09CB\u0997 \u0995\u09B0\u09C1\u09A8."
title: "\u099F\u09AE\u09B2 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\
  \u09B0\u09BE"
weight: 39
---

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
