---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:28.151295-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Clojure \u098F, \u098F\u0995\u099F\
  \u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\u09C7 \u099B\u09CB\u099F\
  \ \u09B9\u09BE\u09A4\u09C7\u09B0 \u0985\u0995\u09CD\u09B7\u09B0\u09C7 \u09B0\u09C2\
  \u09AA\u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\u09B0\u09A4\u09C7, \u0986\u09AA\u09A8\
  \u09BF `clojure.string/lower-case` \u09AB\u09BE\u0982\u09B6\u09A8 \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09AC\u09C7\u09A8\u0964 \u09A6\u09C7\u0996\
  \u09C1\u09A8 \u098F\u099F\u09BE \u0995\u09A4 \u09B8\u09B9\u099C."
lastmod: '2024-03-17T18:47:43.606442-06:00'
model: gpt-4-0125-preview
summary: "Clojure \u098F, \u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982\u0995\u09C7 \u099B\u09CB\u099F \u09B9\u09BE\u09A4\u09C7\u09B0 \u0985\u0995\
  \u09CD\u09B7\u09B0\u09C7 \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\u09B0 \u0995\
  \u09B0\u09A4\u09C7, \u0986\u09AA\u09A8\u09BF `clojure.string/lower-case` \u09AB\u09BE\
  \u0982\u09B6\u09A8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09AC\
  \u09C7\u09A8\u0964 \u09A6\u09C7\u0996\u09C1\u09A8 \u098F\u099F\u09BE \u0995\u09A4\
  \ \u09B8\u09B9\u099C."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0995\u09C7 \u09B2\u09CB\u09AF\u09BC\
  \u09BE\u09B0 \u0995\u09C7\u09B8\u09C7 \u09B0\u09C2\u09AA\u09BE\u09A8\u09CD\u09A4\
  \u09B0 \u0995\u09B0\u09BE"
weight: 4
---

## কিভাবে:
Clojure এ, একটি স্ট্রিংকে ছোট হাতের অক্ষরে রূপান্তর করতে, আপনি `clojure.string/lower-case` ফাংশন ব্যবহার করবেন। দেখুন এটা কত সহজ:

```clojure
(require '[clojure.string :as str])

(str/lower-case "Hello, World!") ; => "hello, world!"
```

আউটপুটটি সোজাসাপটা:

```clojure
"hello, world!"
```

## গভীর ডুব
ঐতিহাসিকভাবে, কেস রূপান্তর প্রথম কম্পিউটিং দিনগুলো থেকেই টেক্সট ডেটা প্রসেসিংকে সামঞ্জস্যপূর্ণ করতে উপস্থিত। Clojure এ, `clojure.string/lower-case` ফাংশনটি `clojure.string` লাইব্রেরীর একটি অংশ, যা মূল ভাষায় অন্তর্ভুক্ত একটি স্ট্রিং ম্যানিপুলেশনের জন্য ইউটিলিটি সমূহের সংগ্রহ।

`clojure.string/lower-case` এর বিকল্পগুলি অন্তর্ভুক্ত আপনার নিজের ফাংশন তৈরি করা `char` ম্যানিপুলেশনের মাধ্যমে ম্যাপিং করা, কিন্তু যখন আপনার কাছে একটি অপ্টিমাইজড এবং ভালভাবে পরীক্ষিত নির্মিত ফাংশন রয়েছে তখন এটি চাকা পুনঃআবিষ্কার করা।

অভ্যন্তরীণভাবে, `clojure.string/lower-case` Java এর নিজস্ব `toLowerCase` মেথডে ভারী কাজটি হস্তান্তর করে, যেহেতু Clojure জাভা ভার্চুয়াল মেশিন (JVM) এ চলে। এটি Java এর পরিপক্ক লাইব্রেরিগুলি কাজে লাগিয়ে উচ্চ কর্মক্ষমতা নিশ্চিত করে।

## আরও দেখুন
- Clojure এর `clojure.string` API: https://clojuredocs.org/clojure.string
- Java এর `String.toLowerCase()` মেথড: https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#toLowerCase()
