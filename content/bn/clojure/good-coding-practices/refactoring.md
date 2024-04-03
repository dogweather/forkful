---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:11:36.359578-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Clojure-\u098F \u09B0\u09BF\u09AB\
  \u09CD\u09AF\u09BE\u0995\u09CD\u099F\u09B0\u09BF\u0982\u2014\u098F\u09B0 \u09AA\u09B0\
  \u09BF\u09B7\u09CD\u0995\u09BE\u09B0 \u09B8\u09BF\u09A8\u099F\u09CD\u09AF\u09BE\u0995\
  \u09CD\u09B8 \u098F\u09AC\u0982 \u09AB\u09BE\u0982\u09B6\u09A8\u09BE\u09B2 \u09AA\
  \u09CD\u09AF\u09BE\u09B0\u09BE\u09A1\u09BE\u0987\u09AE\u09C7\u09B0 \u0995\u09BE\u09B0\
  \u09A3\u09C7\u2014\u0985\u09A4\u09CD\u09AF\u09A8\u09CD\u09A4 \u09B8\u09B0\u09B2\
  \ \u09B9\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u0964 \u099A\u09B2\u09C1\u09A8 \u098F\
  \u0995\u099F\u09BF \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3 \u09AA\u09B0\u09BF\u09B8\
  \u09CD\u09A5\u09BF\u09A4\u09BF \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C\
  \ \u0995\u09B0\u09BE \u09AF\u09BE\u0995:\u2026"
lastmod: '2024-03-17T18:47:43.630191-06:00'
model: gpt-4-0125-preview
summary: "Clojure-\u098F \u09B0\u09BF\u09AB\u09CD\u09AF\u09BE\u0995\u09CD\u099F\u09B0\
  \u09BF\u0982\u2014\u098F\u09B0 \u09AA\u09B0\u09BF\u09B7\u09CD\u0995\u09BE\u09B0\
  \ \u09B8\u09BF\u09A8\u099F\u09CD\u09AF\u09BE\u0995\u09CD\u09B8 \u098F\u09AC\u0982\
  \ \u09AB\u09BE\u0982\u09B6\u09A8\u09BE\u09B2 \u09AA\u09CD\u09AF\u09BE\u09B0\u09BE\
  \u09A1\u09BE\u0987\u09AE\u09C7\u09B0 \u0995\u09BE\u09B0\u09A3\u09C7\u2014\u0985\u09A4\
  \u09CD\u09AF\u09A8\u09CD\u09A4 \u09B8\u09B0\u09B2 \u09B9\u09A4\u09C7 \u09AA\u09BE\
  \u09B0\u09C7\u0964 \u099A\u09B2\u09C1\u09A8 \u098F\u0995\u099F\u09BF \u09B8\u09BE\
  \u09A7\u09BE\u09B0\u09A3 \u09AA\u09B0\u09BF\u09B8\u09CD\u09A5\u09BF\u09A4\u09BF\
  \ \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE \u09AF\u09BE\
  \u0995."
title: "\u09B0\u09BF\u09AB\u09CD\u09AF\u09BE\u0995\u09CD\u099F\u09B0\u09BF\u0982"
weight: 19
---

## কিভাবে:
Clojure-এ রিফ্যাক্টরিং—এর পরিষ্কার সিনট্যাক্স এবং ফাংশনাল প্যারাডাইমের কারণে—অত্যন্ত সরল হতে পারে। চলুন একটি সাধারণ পরিস্থিতি নিয়ে কাজ করা যাক: সংগ্রহসমূহের উপর পুনরাবৃত্তি। আপনি হয়তো `for` লুপ দিয়ে শুরু করবেন, এরকম:

```clojure
(defn calculate-sum [numbers]
  (reduce + 0 numbers))

(defn old-way []
  (let [nums (range 1 11)]
    (calculate-sum nums)))
```

`(old-way)` কল করলে আমরা 55 পাব, 1 থেকে 10 পর্যন্ত যোগফল। কিন্তু, হে, আমরা এটিকে আরো Clojure-এস্ক করে রিফ্যাক্টর করতে পারি:

```clojure
(defn new-way []
  (->> (range 1 11)
       (reduce +)))
```

এই রিফ্যাক্টর করা `(new-way)` ফাংশনে থ্রেডিং ম্যাক্রোজ ব্যবহার করে `reduce`-এ সীমাবদ্ধতা সরাসরি পাস করা হয়েছে, অতিরিক্ত ফ্যাট ট্রিম করে।

## গভীর ডাইভ
রিফ্যাক্টরিং শিল্পের মূল রয়েছে সফ্টওয়্যার উন্নয়নের প্রাথমিক দিনগুলিতে, কিন্তু মার্টিন ফাউলারের "Refactoring: Improving the Design of Existing Code" নামক গ্রন্থের প্রকাশনা দ্বারা সত্যিকার অর্থে জনপ্রিয় হয়ে উঠেছিল 1999 সালে। Clojure-এ, রিফ্যাক্টরিং প্রায়ই ফাংশনাল প্রোগ্রামিং নীতিমালায় ভর করে, বিশুদ্ধ ফাংশন এবং অপরিবর্তনীয় ডেটাস্ট্রাকচার পছন্দ করে।

Clojure-এ ম্যানুয়াল রিফ্যাক্টরিংয়ের বিকল্প হতে পারে Cursive মত টুলস ব্যবহার করা, যা একটি জনপ্রিয় IntelliJ IDEA প্লাগইন, যা Clojure বিশেষজ্ঞ অটোমেটেড রিফ্যাক্টর অফার করে। এছাড়াও clj-refactor রয়েছে, একটি Emacs প্যাকেজ জন্য Clojure, যা রিফ্যাক্টরিং ফাংশনের একটি সুইট প্রদান করে।

Clojure-এ রিফ্যাক্টরিং করার একটি বিশেষ চ্যালেঞ্জ হল রাষ্ট্র এবং পার্শ্ব-প্রভাবের সাথে মোকাবিলা করা, মূলত অপরিবর্তনীয় এবং পার্শ্ব-প্রভাব মুক্ত প্যারাডাইমে। রিফ্যাক্টরিংয়ের সময় দুই পারফরমেন্স এবং সঠিকতা বজায় রাখার জন্য অ্যাটমস, রেফস, এজেন্টস, এবং ট্রানসিয়েন্টসের সাবধানশীল ব্যবহার অত্যন্ত জরুরী।

## আরো দেখুন
- মার্টিন ফাউলারের "Refactoring: Improving the Design of Existing Code" মৌলিক ধারণাগুলির জন্য।
- [Clojure Docs](https://clojuredocs.org/) বিশেষ উদাহরণের জন্য যা Clojure কোডের রীতিনীতির তুলনা করে।
- [clj-refactor](https://github.com/clojure-emacs/clj-refactor.el) Emacs ব্যবহারকারীদের জন্য রিফ্যাক্টরিং অটোমেশন।
- [Cursive](https://cursive-ide.com/) IntelliJ ব্যবহারকারীদের জন্য অটোমেটেড রিফ্যাক্টরিং সহায়তা সন্ধানে।
- [Refactoring with Rich Hickey](https://www.infoq.com/presentations/Simple-Made-Easy/) - Clojure-এর স্রষ্টা দ্বারা একটি আলোচনা যা, রিফ্যাক্টরিং সম্পর্কে না হলেও, Clojure দর্শনের দৃষ্টিভঙ্গি প্রদান করে, যা কার্যকরী রিফ্যাক্টরিং সিদ্ধান্ত গ্রহণে গাইড করে।
