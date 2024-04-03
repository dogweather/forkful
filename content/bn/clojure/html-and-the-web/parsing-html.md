---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:04:40.767062-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Clojure-\u098F HTML \u09AA\u09BE\
  \u09B0\u09CD\u09B8\u09BF\u0982 \u098F\u09B0 \u099C\u09A8\u09CD\u09AF \u0995\u09CB\
  \u09A8\u09CB \u09A8\u09BF\u09B0\u09CD\u09AE\u09BF\u09A4 \u0995\u09CD\u09B7\u09AE\
  \u09A4\u09BE \u09A8\u09C7\u0987, \u0995\u09BF\u09A8\u09CD\u09A4\u09C1 \u0986\u09AA\
  \u09A8\u09BF Java \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u0997\u09C1\
  \u09B2\u09CB \u09AC\u09BE Clojure \u09B0\u200D\u09CD\u09AF\u09BE\u09AA\u09BE\u09B0\
  \ \u09AF\u09C7\u09AE\u09A8 `enlive` \u09AC\u09BE `hickory` \u09AC\u09CD\u09AF\u09AC\
  \u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\u2026"
lastmod: '2024-03-17T18:47:43.619019-06:00'
model: gpt-4-0125-preview
summary: "Clojure-\u098F HTML \u09AA\u09BE\u09B0\u09CD\u09B8\u09BF\u0982 \u098F\u09B0\
  \ \u099C\u09A8\u09CD\u09AF \u0995\u09CB\u09A8\u09CB \u09A8\u09BF\u09B0\u09CD\u09AE\
  \u09BF\u09A4 \u0995\u09CD\u09B7\u09AE\u09A4\u09BE \u09A8\u09C7\u0987, \u0995\u09BF\
  \u09A8\u09CD\u09A4\u09C1 \u0986\u09AA\u09A8\u09BF Java \u09B2\u09BE\u0987\u09AC\u09CD\
  \u09B0\u09C7\u09B0\u09BF\u0997\u09C1\u09B2\u09CB \u09AC\u09BE Clojure \u09B0\u200D\
  \u09CD\u09AF\u09BE\u09AA\u09BE\u09B0 \u09AF\u09C7\u09AE\u09A8 `enlive` \u09AC\u09BE\
  \ `hickory` \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u098F\
  \u099F\u09BE \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\u0964 \u098F\
  \u0996\u09BE\u09A8\u09C7 \u0989\u09AD\u09AF\u09BC\u09C7\u09B0 \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0 \u0995\u09BF\u09AD\u09BE\u09AC\u09C7 \u0995\u09B0\u09AC\
  \u09C7\u09A8 \u09A4\u09BE \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2\u0983\
  \n\n#."
title: "HTML \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE"
weight: 43
---

## কিভাবে:
Clojure-এ HTML পার্সিং এর জন্য কোনো নির্মিত ক্ষমতা নেই, কিন্তু আপনি Java লাইব্রেরিগুলো বা Clojure র‍্যাপার যেমন `enlive` বা `hickory` ব্যবহার করে এটা করতে পারেন। এখানে উভয়ের ব্যবহার কিভাবে করবেন তা দেওয়া হলঃ

### Enlive ব্যবহার করে:
Enlive HTML পার্স এবং ওয়েব স্ক্রেপিং এর জন্য একটি জনপ্রিয় পছন্দ। প্রথমে, আপনার প্রজেক্ট নির্ভরতায় এটি যোগ করুন:

```clojure
[net.cgrand/enlive "1.1.6"]
```

তারপর, HTML পার্স এবং নেভিগেট করা যাবে এরকম:

```clojure
(require '[net.cgrand.enlive-html :as html])

(let [doc (html/html-resource (java.net.URL. "http://example.com"))]
  (html/select doc [:div.some-class]))
```

এই স্নিপেট একটি HTML পাতা ফেচ করে এবং `some-class` ক্লাস সহ সব `<div>` এলিমেন্ট নির্বাচন করে।

আউটপুট হতে পারে:

```clojure
({:tag :div, :attrs {:class "some-class"}, :content ["Here's some content."]})
```

### Hickory ব্যবহার করে:
Hickory Clojure-এ কাজ করার জন্য আরো সহজ ফরম্যাটে HTML পার্স করার একটি উপায় প্রদান করে। Hickory আপনার প্রজেক্ট নির্ভরতায় যোগ করুন:

```clojure
[hickory "0.7.1"]
```

এখানে একটি সহজ উদাহরণ দেওয়া হচ্ছে:

```clojure
(require '[hickory.core :as hickory]
         '[hickory.select :as select])

;; HTML কে Hickory ফর্ম্যাটে পার্স করুন
(let [doc (hickory/parse "<html><body><div id='main'>Hello, world!</div></body></html>")]
  ;; 'main' id সহ div নির্বাচন করুন
  (select/select (select/id "main") doc))
```

এই কোড একটি সাধারণ HTML স্ট্রিং পার্স করে এবং `main` ID সহ একটি `div` খুঁজে পায়।

নমুনা আউটপুট:

```clojure
[{:type :element, :tag :div, :attrs {:id "main"}, :content ["Hello, world!"]}]
```

`enlive` এবং `hickory` উভয়ই Clojure-এ HTML পার্সিং এর জন্য শক্তিশালী সমাধান প্রস্তাব করে, `enlive` টেমপ্লেটিং এর উপর এবং `hickory` ডেটা ট্রান্সফরমেশন এর উপর জোর দেয়।
