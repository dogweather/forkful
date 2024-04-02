---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:27:50.929419-06:00
description: "CSV (\u0995\u09AE\u09BE-\u09AA\u09C3\u09A5\u0995\u09C0\u0995\u09C3\u09A4\
  \ \u09AE\u09BE\u09A8) \u09AB\u09BE\u0987\u09B2\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7\
  \ \u0995\u09BE\u099C \u0995\u09B0\u09BE \u0985\u09B0\u09CD\u09A5 \u09AA\u09BE\u09B0\
  \u09CD\u09B8\u09BF\u0982 \u098F\u09AC\u0982 \u099F\u09C7\u0995\u09CD\u09B8\u099F\
  \ \u09A1\u09C7\u099F\u09BE \u09B8\u0982\u0997\u09A0\u09BF\u09A4 \u0995\u09B0\u09BE\
  \ \u09AF\u09BE \u09B8\u09BE\u09B0\u09BF \u098F\u09AC\u0982 \u09B8\u09CD\u09A4\u09AE\
  \u09CD\u09AD\u09C7\u09B0 \u09AE\u09A4\u09CB, \u09B8\u09CD\u09AA\u09CD\u09B0\u09C7\
  \u09A1\u09B6\u09C0\u099F \u09A1\u09C7\u099F\u09BE\u09B0 \u09AE\u09A4\u09CB\u0964\
  \ \u098F\u0987 \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE\u099F\
  \u09BF\u2026"
lastmod: '2024-03-17T18:47:43.645846-06:00'
model: gpt-4-0125-preview
summary: "CSV (\u0995\u09AE\u09BE-\u09AA\u09C3\u09A5\u0995\u09C0\u0995\u09C3\u09A4\
  \ \u09AE\u09BE\u09A8) \u09AB\u09BE\u0987\u09B2\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7\
  \ \u0995\u09BE\u099C \u0995\u09B0\u09BE \u0985\u09B0\u09CD\u09A5 \u09AA\u09BE\u09B0\
  \u09CD\u09B8\u09BF\u0982 \u098F\u09AC\u0982 \u099F\u09C7\u0995\u09CD\u09B8\u099F\
  \ \u09A1\u09C7\u099F\u09BE \u09B8\u0982\u0997\u09A0\u09BF\u09A4 \u0995\u09B0\u09BE\
  \ \u09AF\u09BE \u09B8\u09BE\u09B0\u09BF \u098F\u09AC\u0982 \u09B8\u09CD\u09A4\u09AE\
  \u09CD\u09AD\u09C7\u09B0 \u09AE\u09A4\u09CB, \u09B8\u09CD\u09AA\u09CD\u09B0\u09C7\
  \u09A1\u09B6\u09C0\u099F \u09A1\u09C7\u099F\u09BE\u09B0 \u09AE\u09A4\u09CB\u0964\
  \ \u098F\u0987 \u09AA\u09CD\u09B0\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\u09BE\u099F\
  \u09BF\u2026"
title: "CSV \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
weight: 37
---

## কি এবং কেন?

CSV (কমা-পৃথকীকৃত মান) ফাইলের সাথে কাজ করা অর্থ পার্সিং এবং টেক্সট ডেটা সংগঠিত করা যা সারি এবং স্তম্ভের মতো, স্প্রেডশীট ডেটার মতো। এই প্রক্রিয়াটি অ্যাপ্লিকেশন, ডাটাবেস এবং ডেটা রূপান্তর করার কাজের মধ্যে তথ্য বিনিময়ের জন্য অপরিহার্য, কারণ CSV একটি প্রসারিত, হালকা ও সহজলভ্যা ফর্ম্যাট হিসেবে ব্যাপকভাবে গৃহীত।

## কিভাবে:

### একটি CSV ফাইল পড়া
ক্লোজারে তার স্ট্যান্ডার্ড লাইব্রেরিতে নিজস্ব CSV পার্সিং নেই, তবে এর জন্য আপনি `clojure.data.csv` লাইব্রেরি ব্যবহার করতে পারেন। প্রথমে, আপনার প্রজেক্ট নির্ভরতায় এই লাইব্রেরিটি যোগ করুন।

আপনার `project.clj`-এ নিম্নলিখিত নির্ভরতা যোগ করুন:
```clojure
[clojure.data.csv "1.0.0"]
```
একটি CSV ফাইল পড়তে এবং প্রতিটি সারি প্রিন্ট করতে:
```clojure
(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(with-open [reader (io/reader "path/to/yourfile.csv")]
  (doall
   (map println (csv/read-csv reader))))
```
এতে CSV-র প্রতিটি সারি একটি ক্লোজার ভেক্টর হিসেবে আউটপুট হবে।

### একটি CSV ফাইলে লিখন
ডেটা একটি CSV ফাইলে লেখার জন্য, আপনি একই `clojure.data.csv` লাইব্রেরি ব্যাবহার করতে পারেন:
```clojure
(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(let [data [["id" "name" "age"]
            ["1" "John Doe" "28"]
            ["2" "Jane Doe" "31"]]]
  (with-open [writer (io/writer "path/to/outputfile.csv")]
    (csv/write-csv writer data)))
```
এটি `outputfile.csv` তৈরি বা অধিভূত করে নির্দিষ্ট ডেটা দিয়ে ভরাট করবে।

### থার্ড-পার্টি লাইব্রেরি ব্যবহার: `clojure.data.csv`

যদিও `clojure.data.csv` হল ক্লোজারে CSV সম্পর্কিত কাজে সর্বাধিক সরল লাইব্রেরি, বিশেষ অক্ষর বা অস্বাভাবিক ডিলিমিটারযুক্ত CSV-সংক্রান্ত আরো জটিল কাজের জন্য, আপনি ইকোসিস্টেমের মধ্যে অতিরিক্ত বিকল্প অন্বেষণ করতে পারেন অথবা এমনকি অ্যাপাচি কমন্স CSV-র মতো লাইব্রেরিগুলির সাথে জাভা ইন্টারঅপের বিবেচনা করতে পারেন। তবে, ক্লোজারে স্ট্যান্ডার্ড CSV প্রসেসিং কাজের জন্য, `clojure.data.csv` একটি সহজ এবং কার্যকর টুলসেট প্রদান করে।
