---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:20:46.597942-06:00
description: "\u09A8\u09A4\u09C1\u09A8 \u098F\u0995\u099F\u09BF \u09AA\u09CD\u09B0\
  \u099C\u09C7\u0995\u09CD\u099F \u09B6\u09C1\u09B0\u09C1 \u0995\u09B0\u09BE \u09AE\
  \u09BE\u09A8\u09C7 \u0986\u09AA\u09A8\u09BE\u09B0 \u0995\u09CB\u09A1\u09C7\u09B0\
  \ \u099C\u09A8\u09CD\u09AF \u098F\u0995\u099F\u09BF \u09A8\u09A4\u09C1\u09A8 \u09AA\
  \u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BF\u0982 \u09AA\u09B0\u09BF\u09AC\
  \u09C7\u09B6 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF\
  \ \u09AC\u09BF\u0995\u09BE\u09B6\u09C7\u09B0 \u09B6\u09C1\u09B0\u09C1\u09A4\u09C7\
  \ \u098F\u0995\u099F\u09BF \u09AA\u09B0\u09BF\u09B7\u09CD\u0995\u09BE\u09B0 \u09B8\
  \u09CD\u09B2\u09C7\u099F \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C\u2026"
lastmod: '2024-03-17T18:47:43.622027-06:00'
model: gpt-4-0125-preview
summary: "\u09A8\u09A4\u09C1\u09A8 \u098F\u0995\u099F\u09BF \u09AA\u09CD\u09B0\u099C\
  \u09C7\u0995\u09CD\u099F \u09B6\u09C1\u09B0\u09C1 \u0995\u09B0\u09BE \u09AE\u09BE\
  \u09A8\u09C7 \u0986\u09AA\u09A8\u09BE\u09B0 \u0995\u09CB\u09A1\u09C7\u09B0 \u099C\
  \u09A8\u09CD\u09AF \u098F\u0995\u099F\u09BF \u09A8\u09A4\u09C1\u09A8 \u09AA\u09CD\
  \u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BF\u0982 \u09AA\u09B0\u09BF\u09AC\u09C7\
  \u09B6 \u09A4\u09C8\u09B0\u09BF \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\
  \u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u098F\u099F\u09BF \u09AC\
  \u09BF\u0995\u09BE\u09B6\u09C7\u09B0 \u09B6\u09C1\u09B0\u09C1\u09A4\u09C7 \u098F\
  \u0995\u099F\u09BF \u09AA\u09B0\u09BF\u09B7\u09CD\u0995\u09BE\u09B0 \u09B8\u09CD\
  \u09B2\u09C7\u099F \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C\u2026"
title: "\u09A8\u09A4\u09C1\u09A8 \u09AA\u09CD\u09B0\u0995\u09B2\u09CD\u09AA \u09B6\
  \u09C1\u09B0\u09C1 \u0995\u09B0\u09BE"
weight: 1
---

## কি এবং কেন?

নতুন একটি প্রজেক্ট শুরু করা মানে আপনার কোডের জন্য একটি নতুন প্রোগ্রামিং পরিবেশ তৈরি করা। প্রোগ্রামাররা এটি বিকাশের শুরুতে একটি পরিষ্কার স্লেট নিয়ে কাজ শুরু করার এবং তাদের চিন্তাগুলিকে স্পর্শযোগ্য কোডে পরিণত করার জন্য করে থাকে।

## কিভাবে:

একটি Clojure প্রজেক্ট বুটস্ট্র্যাপ করতে, আমরা Leiningen ব্যবহার করব, যা Clojure এর জন্য একটি জনপ্রিয় বিল্ড টুল:

``` Clojure
;; ১. Leiningen ইনস্টল করুন যদি আপনি পূর্বে না করে থাকেন (https://leiningen.org/)
;; ২. একটি নতুন প্রজেক্ট কঙ্কাল তৈরি করুন:
lein new app my-cool-app

;; ৩. আপনার নতুন প্রজেক্টে নেভিগেট করুন:
cd my-cool-app

;; ৪. একটি REPL (Read-Eval-Print Loop) শুরু করুন:
lein repl

;; নমুনা আউটপুট:
;; nREPL সার্ভার 12345 পোর্টে 127.0.0.1 হোস্টে শুরু হয়েছে - nrepl://127.0.0.1:12345
;; REPL-y 0.4.4, nREPL 0.6.0
;; Clojure 1.10.1
;; Java 1.8.0_232
;;     ডক্স: (doc function-name-here)
;;           (find-doc "part-of-name-here")
;;   সোর্স: (source function-name-here)
;;  জাভাডক: (javadoc java-object-or-class-here)
;;     এক্সিট: Control+D অথবা (exit) অথবা (quit)
;;  ফলাফল: *1, *2, *3 ভ্যারেবলগুলিতে সংরক্ষিত, এবং ব্যতিক্রম *e তে

;; ৫. আপনার কোডের জন্য একটি ফাইল তৈরি করুন (src/my_cool_app/core.clj) এবং এটিকে আপনার পছন্দের টেক্সট এডিটরে খুলুন।

;; ৬. কিছু সাধারণ Clojure কোড লিখুন:
(ns my-cool-app.core)

(defn say-hello []
  (println "Hello, Clojure world!"))

;; ৭. REPL এ আপনার ফাংশন রান করুন:
(my-cool-app.core/say-hello)

;; নমুনা আউটপুট:
;; Hello, Clojure world!
```

## গভীর ডুব

Clojure প্রজেক্টগুলি প্রায়শই Leiningen অথবা Boot দিয়ে শুরু হয়, যা নির্ভরশীলতা পরিচালনা, বিল্ডিং, এবং কাজ অটোমেট করার জন্য ব্যবহৃত হয়। Leiningen ২০১০ সালে চালু হয়েছিল এবং এটি বেশিরভাগ Clojurist এর জন্য ডিফল্ট পছন্দ হয়ে উঠেছে।

বিকল্প টুলস যেমন `deps.edn` এবং Clojure CLI টুলস আছে, যা Clojure/core কর্তৃক আরও সরল নির্ভরশীলতা পরিচালনা এবং প্রজেক্ট কনফিগারেশন প্রদান করার জন্য চালু করা হয়েছিল।

Clojure নিজেই অপরিবর্তনীয়তা এবং ফাংশনাল প্রোগ্রামিংকে মূল্যায়িত করে। একটি প্রজেক্ট সঠিকভাবে শুরু করা মানে পরিষ্কার স্টেট ম্যানেজমেন্ট এবং ফাংশন এবং নেমস্পেসগুলি জুড়ে উদ্বেগের পৃথকীকরণ জোর দেওয়া।

প্রোজেক্টগুলি সাধারণত একটি মানক ডিরেক্টরি কাঠামো অনুসরণ করে:
- `src/` আপনার মূল কোডের জন্য।
- `test/` টেস্ট কোডের জন্য।
- `resources/` নন-কোড রিসোর্সের জন্য।
- `project.clj` অথবা `deps.edn` নির্ভরশীলতা এবং কনফিগারেশন পরিচালনার জন্য।

শুরু থেকে জিনিসগুলিকে সর্বনিম্নে রাখা ভালো অভ্যাস। আপনার প্রোজেক্টটিকে হালকা এবং পরিচালনাযোগ্য রাখার জন্য আপনি যেতে যেতে নির্ভরশীলতা যোগ করুন।

## আরও দেখুন

- [Leiningen's গেটিং স্টার্টেড গাইড](https://leiningen.org/#getting-started)
- [Clojure ডক্স](https://clojuredocs.org/)
- [Clojure স্টাইল গাইড](https://guide.clojure.style/)
- [Clojure CLI টুলস](https://clojure.org/guides/getting_started)
- [The Clojure টুলবক্স](https://www.clojure-toolbox.com/)
