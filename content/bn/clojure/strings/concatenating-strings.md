---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:23.985387-06:00
description: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0995\u09A8\u0995\u09CD\u09AF\
  \u09BE\u099F\u09C7\u09A8\u09C7\u09B6\u09A8 \u09AE\u09BE\u09A8\u09C7 \u09A4\u09BE\
  \u09A6\u09C7\u09B0 \u09B6\u09C7\u09B7\u09BE\u0982\u09B6\u0997\u09C1\u09B2\u09BF\
  \ \u098F\u0995\u09C7 \u0985\u09AA\u09B0\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u09AF\
  \u09C1\u0995\u09CD\u09A4 \u0995\u09B0\u09BE - \"hello\" + \"world\" \u09B9\u09DF\
  \u09C7 \u09AF\u09BE\u09DF \"helloworld\"\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\
  \u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u0987\u0989\u0986\u09B0\u098F\u09B2\
  , \u09AC\u09BE\u09B0\u09CD\u09A4\u09BE, \u0985\u09A5\u09AC\u09BE\u2026"
lastmod: '2024-03-17T18:47:43.612575-06:00'
model: gpt-4-0125-preview
summary: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0995\u09A8\u0995\u09CD\u09AF\
  \u09BE\u099F\u09C7\u09A8\u09C7\u09B6\u09A8 \u09AE\u09BE\u09A8\u09C7 \u09A4\u09BE\
  \u09A6\u09C7\u09B0 \u09B6\u09C7\u09B7\u09BE\u0982\u09B6\u0997\u09C1\u09B2\u09BF\
  \ \u098F\u0995\u09C7 \u0985\u09AA\u09B0\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u09AF\
  \u09C1\u0995\u09CD\u09A4 \u0995\u09B0\u09BE - \"hello\" + \"world\" \u09B9\u09DF\
  \u09C7 \u09AF\u09BE\u09DF \"helloworld\"\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\
  \u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u0987\u0989\u0986\u09B0\u098F\u09B2\
  , \u09AC\u09BE\u09B0\u09CD\u09A4\u09BE, \u0985\u09A5\u09AC\u09BE \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0\u0995\u09BE\u09B0\u09C0\u09B0 \u0987\u09A8\u09AA\u09C1\u099F\
  \ \u0985\u09A5\u09AC\u09BE \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\
  \u09C7\u09B0 \u09A1\u09BE\u099F\u09BE \u09AD\u09BF\u09A4\u09CD\u09A4\u09BF\u0995\
  \ \u09AB\u09B2\u09BE\u09AB\u09B2 \u09A4\u09C8\u09B0\u09BF\u09B0 \u099C\u09A8\u09CD\
  \u09AF \u098F\u099F\u09BF \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09C7\u09A8\u0964\
  ."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u099C\u09CB\u09A1\u09BC\u09BE\
  \ \u09A6\u09C7\u0993\u09AF\u09BC\u09BE"
weight: 3
---

## কি এবং কেন?

স্ট্রিং কনক্যাটেনেশন মানে তাদের শেষাংশগুলি একে অপরের সাথে যুক্ত করা - "hello" + "world" হয়ে যায় "helloworld"। প্রোগ্রামাররা ইউআরএল, বার্তা, অথবা ব্যবহারকারীর ইনপুট অথবা প্রোগ্রামের ডাটা ভিত্তিক ফলাফল তৈরির জন্য এটি করে থাকেন।

## কিভাবে:

ক্লোজার স্ট্রিং কনক্যাটেনেশনকে সহজ করে তোলে `str` ফাংশনের মাধ্যমে। চলুন ডুব দিয়ে দেখি:

```clojure
;; str ফাংশনের সাথে সাধারণ কনক্যাটেনেশন
(str "Hello, " "world!")
;; => "Hello, world!"

;; একাধিক স্ট্রিং কনক্যাটেনেটিং
(str "Clojure" " is" " awesome!")
;; => "Clojure is awesome!"

;; স্ট্রিং এবং অন্যান্য মানগুলি কম্বাইন করা
(str "The answer is " 42)
;; => "The answer is 42"

;; স্ট্রিংয়ের একটি সিকোয়েন্স কনক্যাটেনেট করতে apply ব্যবহার করা
(apply str ["Join" " " "these" " " "strings!"])
;; => "Join these strings!"
```

দারুণ, তাহলে আপনি এটি কাজে লাগানো দেখলেন। মনে রাখবেন `str` যেকোনো মান নিয়ে কাজ করে `toString` ডাকার মাধ্যমে। যদি এটি nil হয়, আপনি "nil" স্ট্রিং পাবেন।

## গভীর ডাইভ

ঐতিহাসিকভাবে, স্ট্রিং কনক্যাটেনেশন প্রোগ্রাম্যাটিকালি টেক্সট সম্পাদনার প্রয়োজন হলে অস্তিত্বে এসেছিল, এবং প্রতিটি ভাষা নিজের উপায়গুলি অফার করে। ক্লোজারে, `str` মৌলিক লাইব্রেরিতে অন্তর্ভুক্ত, সরলতা এবং একত্রীকরণের জন্য প্রবর্তিত হয়েছিল।

`str`-এর বিকল্প? হ্যাঁ! `StringBuilder` বহুগুণ কনক্যাটেনেশনের জন্য আরও দক্ষ হতে পারে, বিশেষ করে লুপে। ক্লোজার জাভা মেথডগুলি কল করতে পারে, তাই আপনি `StringBuilder`ও ব্যবহার করতে পারেন:

```clojure
;; দক্ষতা বৃদ্ধির জন্য StringBuilder ব্যবহার করা
(let [builder (StringBuilder.)]
  (.append builder "This is")
  (.append builder " a more")
  (.append builder " efficient way!")
  (.toString builder))
;; => "This is a more efficient way!"
```

তাহলে সর্বদা `StringBuilder` ব্যবহার করা হয় না কেন? বেশিরভাগ সহজ কাজের জন্য, `str` সহজ এবং যথেষ্ট দ্রুত। `StringBuilder` অনেকগুলো কনক্যাটেনেশনের উচ্চ-পারফরম্যান্স সিনারিওগুলিতে উজ্জ্বল হয়।

বাস্তবায়নের দিক থেকে, যেহেতু ক্লোজার JVM এ হোস্ট করা হয়, তাই এটি জাভার স্ট্রিং হ্যান্ডলিং ক্ষমতা থেকে উপকার পায়। তবে, জাভা `String`-এর মত, প্রতিটি `str` কল একটি নতুন `String` তৈরি করে, যা একটি মেমোরি বিবেচনা হতে পারে।

## দেখুন এছাড়াও

- ক্লোজারের `str` ফাংশন ডকুমেন্টেশন: [Clojure Strings](https://clojuredocs.org/clojure.core/str)
- জাভার `StringBuilder`: [StringBuilder Docs](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/StringBuilder.html)
- `str` এবং আরও সম্পর্কে প্র্যাকটিকাল ক্লোজার গাইড: [Clojure for the Brave and True](https://www.braveclojure.com/clojure-for-the-brave-and-true/)
