---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:22:15.122775-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Clojure \u099C\u09BE\u09AD\u09BE\
  \ \u09AD\u09BE\u09B0\u09CD\u099A\u09C1\u09AF\u09BC\u09BE\u09B2 \u09AE\u09C7\u09B6\
  \u09BF\u09A8 (JVM) \u098F\u09B0 \u0989\u09AA\u09B0 \u09A8\u09BF\u09B0\u09CD\u09AD\
  \u09B0 \u0995\u09B0\u09C7, \u09A4\u09BE\u0987 \u0985\u09A8\u09C7\u0995 \u09A1\u09BF\
  \u09AC\u09BE\u0997\u09BF\u0982 \u099C\u09BE\u09AD\u09BE \u099F\u09C2\u09B2\u09B8\
  \ \u09A6\u09BF\u09AF\u09BC\u09C7 \u09B9\u09AF\u09BC\u0964 \u098F\u09AE\u09A8 \u098F\
  \u0995\u099F\u09BF \u099F\u09C1\u09B2 \u09B9\u09B2 `CIDER`, Emacs-\u098F Clojure\
  \ \u09AC\u09BF\u0995\u09BE\u09B6\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF\u2026"
lastmod: '2024-03-17T18:47:43.626091-06:00'
model: gpt-4-0125-preview
summary: "Clojure \u099C\u09BE\u09AD\u09BE \u09AD\u09BE\u09B0\u09CD\u099A\u09C1\u09AF\
  \u09BC\u09BE\u09B2 \u09AE\u09C7\u09B6\u09BF\u09A8 (JVM) \u098F\u09B0 \u0989\u09AA\
  \u09B0 \u09A8\u09BF\u09B0\u09CD\u09AD\u09B0 \u0995\u09B0\u09C7, \u09A4\u09BE\u0987\
  \ \u0985\u09A8\u09C7\u0995 \u09A1\u09BF\u09AC\u09BE\u0997\u09BF\u0982 \u099C\u09BE\
  \u09AD\u09BE \u099F\u09C2\u09B2\u09B8 \u09A6\u09BF\u09AF\u09BC\u09C7 \u09B9\u09AF\
  \u09BC\u0964 \u098F\u09AE\u09A8 \u098F\u0995\u099F\u09BF \u099F\u09C1\u09B2 \u09B9\
  \u09B2 `CIDER`, Emacs-\u098F Clojure \u09AC\u09BF\u0995\u09BE\u09B6\u09C7\u09B0\
  \ \u099C\u09A8\u09CD\u09AF \u098F\u0995\u099F\u09BF \u09B6\u0995\u09CD\u09A4\u09BF\
  \u09B6\u09BE\u09B2\u09C0 \u09AA\u09CD\u09AF\u09BE\u0995\u09C7\u099C, \u09AF\u09BE\
  \u09B0 \u09A6\u09C3\u09A2\u09BC \u09A1\u09BF\u09AC\u09BE\u0997\u09BF\u0982 \u0995\
  \u09CD\u09B7\u09AE\u09A4\u09BE \u09B0\u09AF\u09BC\u09C7\u099B\u09C7\u0964 \u099A\
  \u09B2\u09C1\u09A8 \u098F\u09A4\u09C7 \u099D\u09BE\u0981\u09AA\u09BF\u09AF\u09BC\
  \u09C7 \u09AA\u09A1\u09BC\u09BF."
title: "\u09A1\u09BF\u09AC\u09BE\u0997\u09BE\u09B0 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09BE"
weight: 35
---

## কিভাবে:
Clojure জাভা ভার্চুয়াল মেশিন (JVM) এর উপর নির্ভর করে, তাই অনেক ডিবাগিং জাভা টূলস দিয়ে হয়। এমন একটি টুল হল `CIDER`, Emacs-এ Clojure বিকাশের জন্য একটি শক্তিশালী প্যাকেজ, যার দৃঢ় ডিবাগিং ক্ষমতা রয়েছে। চলুন এতে ঝাঁপিয়ে পড়ি:

```clojure
;; প্রথমে, Emacs ব্যবহার করে CIDER ব্যবহার করে একটি Clojure প্রকল্পে জ্যাক-ইন করুন
M-x cider-jack-in

;; একটি ব্রেকপয়েন্ট সেট করুন
;; আপনার Clojure কোডের যে লাইনটি আপনি পরীক্ষা করতে চান তা নেভিগেট করুন এবং
;; "C-c M-b" চাপুন অথবা নিম্নলিখিত কমান্ড চালান:
M-x cider-debug-defun-at-point

;; কোড চালানোর সময়, আপনি ব্রেকপয়েন্টে পৌঁছাবেন। CIDER আপনাকে উপস্থাপন করবে:
;; 1. n পরবর্তী যৌক্তিক পদক্ষেপে যেতে,
;; 2. c পরবর্তী ব্রেকপয়েন্ট পর্যন্ত নিরন্তর চালান হওয়া অব্যাহত রাখতে,
;; 3. q ডিবাগিং ছেড়ে দিতে।

;; ব্রেকপয়েন্টে স্থানীয় চরগুলি পরীক্ষা করুন
;; একটি ব্রেকপয়েন্টে থাকাকালীন, টাইপ করুন:
locals

;; আপনি মিনিবাফারে প্রিন্ট করা স্থানীয় চরগুলি এবং তাদের মানের একটি তালিকা দেখতে পাবেন।
```
স্যাম্পল আউটপুট দেখতে পারে:
```clojure
{:x 10, :y 20, :result 200}
```

## গভীর ডুব
ডিবাগার একটি টুল, যা কম্পিউটিং শব্দে প্রাচীন পাহাড়ের মত। "বাগ" শব্দটি একটি প্রকৃত পতঙ্গ যখন একটি মেশিনের একটি সার্কিটে শর্ট সৃষ্টি করে একটি ত্রুটি সৃষ্টির কারণে কম্পিউটিং-এর প্রাথমিক দিনগুলিতে তৈরি হয়েছিল।

যদিও `CIDER` Emacs উৎসাহীদের জন্য দুর্দান্ত, Clojure ডিবাগিং-এর জন্য বিকল্প রয়েছে। উদাহরণস্বরূপ, IntelliJ এর সাথে Cursive প্লাগিন ব্যবহার করে একটি আরো GUI-চালিত ডিবাগিং অভিজ্ঞতা দিতে পারে। প্লাস, আপনি ডিবাগিং-এর সময় প্রক্রিয়া প্রবাহ নিয়ন্ত্রণ করতে Leiningen অথবা tools.deps ব্যবহার করতে পারেন।

অভ্যন্তরীণভাবে, এই ডিবাগারগুলি প Often অক্ষরে অক্ষরে প্রায়শই ম্যানিপুলেট করে, নির্দিষ্ট nREPL সেশনগুলিতে মূল্যায়ন সম্পাদন করে, এবং স্ট্যাক ট্রেস পরিদর্শন সরবরাহ করে। তারা জাভা ডিবাগিং ফ্রেমওয়ার্কের বিশাল ভাণ্ডারের মধ্যে অভিযোজিত হয়ে অধস্তন JVM-এর ক্ষমতাকে কাজে লাগাচ্ছে।

## আরও দেখুন
- [CIDER ডিবাগার ডকুমেন্টেশন](https://docs.cider.mx/cider/debugging/debugger.html)
- [Cursive ডিবাগার](https://cursive-ide.com/userguide/debugging.html)
- [অটোমেশন এবং ডিবাগিং জন্য Leiningen](https://leiningen.org/)
- [আরও নিয়ন্ত্রণের জন্য tools.deps.alpha](https://github.com/clojure/tools.deps.alpha)
