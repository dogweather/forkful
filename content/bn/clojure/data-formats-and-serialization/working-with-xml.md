---
title:                "XML নিয়ে কাজ করা"
date:                  2024-03-17T18:31:38.370431-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
