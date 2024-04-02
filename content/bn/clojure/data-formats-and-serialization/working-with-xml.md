---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:31:38.370431-06:00
description: "XML \u098F\u0995\u099F\u09BF \u09AE\u09BE\u09B0\u09CD\u0995\u0986\u09AA\
  \ \u09AD\u09BE\u09B7\u09BE \u09AF\u09BE \u09A8\u09A5\u09BF\u0997\u09C1\u09B2\u09BF\
  \u0995\u09C7 \u098F\u09AE\u09A8\u09AD\u09BE\u09AC\u09C7 \u098F\u09A8\u0995\u09CB\
  \u09A1 \u0995\u09B0\u09C7 \u09AF\u09BE \u09AE\u09BE\u09A8\u09C1\u09B7 \u098F\u09AC\
  \u0982 \u09AE\u09C7\u09B6\u09BF\u09A8 \u0989\u09AD\u09AF\u09BC\u09C7\u09B0 \u0995\
  \u09BE\u099B\u09C7\u0987 \u09AA\u09BE\u09A0\u09AF\u09CB\u0997\u09CD\u09AF\u0964\
  \ \u098F\u099F\u09BF \u0993\u09AF\u09BC\u09C7\u09AC \u09B8\u09C7\u09AC\u09BE, \u0995\
  \u09A8\u09AB\u09BF\u0997\u09BE\u09B0\u09C7\u09B6\u09A8 \u09AB\u09BE\u0987\u09B2\
  \ \u098F\u09AC\u0982 \u09A1\u09C7\u099F\u09BE \u0987\u09A8\u09CD\u099F\u09BE\u09B0\
  \u099A\u09C7\u099E\u09CD\u099C\u09C7 \u0995\u09C0\u2026"
lastmod: '2024-03-17T18:47:43.648038-06:00'
model: gpt-4-0125-preview
summary: "XML \u098F\u0995\u099F\u09BF \u09AE\u09BE\u09B0\u09CD\u0995\u0986\u09AA\
  \ \u09AD\u09BE\u09B7\u09BE \u09AF\u09BE \u09A8\u09A5\u09BF\u0997\u09C1\u09B2\u09BF\
  \u0995\u09C7 \u098F\u09AE\u09A8\u09AD\u09BE\u09AC\u09C7 \u098F\u09A8\u0995\u09CB\
  \u09A1 \u0995\u09B0\u09C7 \u09AF\u09BE \u09AE\u09BE\u09A8\u09C1\u09B7 \u098F\u09AC\
  \u0982 \u09AE\u09C7\u09B6\u09BF\u09A8 \u0989\u09AD\u09AF\u09BC\u09C7\u09B0 \u0995\
  \u09BE\u099B\u09C7\u0987 \u09AA\u09BE\u09A0\u09AF\u09CB\u0997\u09CD\u09AF\u0964\
  \ \u098F\u099F\u09BF \u0993\u09AF\u09BC\u09C7\u09AC \u09B8\u09C7\u09AC\u09BE, \u0995\
  \u09A8\u09AB\u09BF\u0997\u09BE\u09B0\u09C7\u09B6\u09A8 \u09AB\u09BE\u0987\u09B2\
  \ \u098F\u09AC\u0982 \u09A1\u09C7\u099F\u09BE \u0987\u09A8\u09CD\u099F\u09BE\u09B0\
  \u099A\u09C7\u099E\u09CD\u099C\u09C7 \u0995\u09C0\u2026"
title: "XML \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
weight: 40
---

## কী এবং কেন?
XML একটি মার্কআপ ভাষা যা নথিগুলিকে এমনভাবে এনকোড করে যা মানুষ এবং মেশিন উভয়ের কাছেই পাঠযোগ্য। এটি ওয়েব সেবা, কনফিগারেশন ফাইল এবং ডেটা ইন্টারচেঞ্জে কী কারণের জন্য এটি ডেটাকে একটি গঠনমূলক, স্তরিক ফর্ম্যাটে বহন করে।

## কিভাবে:
Clojure এক্সএমএল পার্সিং এবং এমিটিংয়ের জন্য `clojure.data.xml` লাইব্রেরি অফার করে। প্রথমে, আসুন কিছু XML পার্স করি:

```clojure
(require '[clojure.data.xml :as xml])

(let [content "<root><foo>bar</foo><foo>baz</foo></root>"
      parsed (xml/parse-str content)] ; XML স্ট্রিং পার্স করুন
  (println parsed))
```
আউটপুট:
```
Element{:tag :root, :attrs {}, :content (Element{:tag :foo, :attrs {}, :content ("bar")} Element{:tag :foo, :attrs {}, :content ("baz")})}
```

Clojure কাঠামো থেকে XML এমিট করার জন্য:

```clojure
(def my-xml (xml/element :root {}
                          (xml/element :foo {} "bar")
                          (xml/element :foo {} "baz")))

(println (xml/emit-str my-xml))
```
আউটপুট:
```
<root><foo>bar</foo><foo>baz</foo></root>
```

## গভীর ডাইভ
XML '90-এর দশকের শেষ ভাগে ওয়েব ডেটার জন্য একটি সরলীকৃত উপশেত্র SGML হিসেবে শুরু হয়েছিল। এটি SOAP এবং XHTML এর মতো প্রযুক্তিগুলির সাথে ব্যবহারে বিস্ফোরণ ঘটেছিল, কিন্তু JSON থেকে এর হালকাতা এবং সহজলভ্যতা পছন্দ করে কিছুটা প্রতিযোগিতা পেয়েছিল।

Clojure-এর XML এর প্রতি দৃষ্টিভঙ্গি এটিকে ফাংশনাল এবং ডেটা-কেন্দ্রিক রেখেছে, ভাষার ইথোসে সত্যিকারের থাকার চেষ্টা করে। `clojure.data.xml` কেবলমাত্র একটি বিকল্প; আপনার কাছে `clojure.xml` বেসিক চাহিদাগুলির জন্য রয়েছে, এবং জাভা ইন্টারঅপের জন্য, আপনি JAXB বা DOM4J এর মতো ভারী-বাহকদের সাথে যেতে পারেন।

মনে রাখবেন, খুব বড় XML নথিগুলি নিয়ে কাজ করার সময় পারফরম্যান্স এবং মেমোরি ওভারহেড বৃহৎ হতে পারে। স্ট্রিমিং পার্সার যেমন StAX সাহায্য করতে পারে, কিন্তু আপনাকে তাদের জন্য জাভা-ল্যান্ডে যেতে হবে।

## দেখুন ও
- [clojure.data.xml GitHub](https://github.com/clojure/data.xml)
- [জাভা API জন্য XML প্রসেসিং (JAXP)](https://docs.oracle.com/javase/tutorial/jaxp/index.html)
- [StAX](https://docs.oracle.com/javase/tutorial/jaxp/stax/index.html)
