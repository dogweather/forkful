---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:36:50.832702-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Clojure \u09A4\u09C7 YAML \u098F\
  \u09B0 \u099C\u09A8\u09CD\u09AF \u0985\u09A8\u09CD\u09A4\u09B0\u09CD\u09A8\u09BF\
  \u09B0\u09CD\u09AE\u09BF\u09A4 \u09B8\u09AE\u09B0\u09CD\u09A5\u09A8 \u0985\u09A8\
  \u09CD\u09A4\u09B0\u09CD\u09AD\u09C1\u0995\u09CD\u09A4 \u09A8\u09C7\u0987, \u09A4\
  \u09AC\u09C7 \u0986\u09AA\u09A8\u09BF `clj-yaml` \u098F\u09B0 \u09AE\u09A4\u09CB\
  \ \u09A4\u09C3\u09A4\u09C0\u09AF\u09BC \u09AA\u0995\u09CD\u09B7\u09C7\u09B0 \u09B2\
  \u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09C7 YAML \u09A1\u09C7\u099F\u09BE \u09AA\u09BE\u09B0\u09CD\
  \u09B8\u09BF\u0982 \u098F\u09AC\u0982\u2026"
lastmod: '2024-03-17T18:47:43.643737-06:00'
model: gpt-4-0125-preview
summary: "Clojure \u09A4\u09C7 YAML \u098F\u09B0 \u099C\u09A8\u09CD\u09AF \u0985\u09A8\
  \u09CD\u09A4\u09B0\u09CD\u09A8\u09BF\u09B0\u09CD\u09AE\u09BF\u09A4 \u09B8\u09AE\u09B0\
  \u09CD\u09A5\u09A8 \u0985\u09A8\u09CD\u09A4\u09B0\u09CD\u09AD\u09C1\u0995\u09CD\u09A4\
  \ \u09A8\u09C7\u0987, \u09A4\u09AC\u09C7 \u0986\u09AA\u09A8\u09BF `clj-yaml` \u098F\
  \u09B0 \u09AE\u09A4\u09CB \u09A4\u09C3\u09A4\u09C0\u09AF\u09BC \u09AA\u0995\u09CD\
  \u09B7\u09C7\u09B0 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u09AC\
  \u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 YAML \u09A1\u09C7\u099F\u09BE\
  \ \u09AA\u09BE\u09B0\u09CD\u09B8\u09BF\u0982 \u098F\u09AC\u0982 \u099C\u09C7\u09A8\
  \u09BE\u09B0\u09C7\u099F\u09BF\u0982 \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\
  \u09C7\u09A8\u0964 \u09AA\u09CD\u09B0\u09A5\u09AE\u09C7, \u0986\u09AA\u09A8\u09BE\
  \u09B0 \u09AA\u09CD\u09B0\u099C\u09C7\u0995\u09CD\u099F\u09C7\u09B0 \u09A1\u09BF\
  \u09AA\u09C7\u09A8\u09CD\u09A1\u09C7\u09A8\u09CD\u09B8\u09BF\u09A4\u09C7 \u09B2\u09BE\
  \u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u099F\u09BF \u09AF\u09CB\u0997 \u0995\
  \u09B0\u09C1\u09A8."
title: "\u0987\u09DF\u09BE\u09AE\u09C7\u09B2 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\
  \u09BE\u099C \u0995\u09B0\u09BE"
weight: 41
---

## কিভাবে:
Clojure তে YAML এর জন্য অন্তর্নির্মিত সমর্থন অন্তর্ভুক্ত নেই, তবে আপনি `clj-yaml` এর মতো তৃতীয় পক্ষের লাইব্রেরি ব্যবহার করে YAML ডেটা পার্সিং এবং জেনারেটিং করতে পারেন। প্রথমে, আপনার প্রজেক্টের ডিপেন্ডেন্সিতে লাইব্রেরিটি যোগ করুন:

```clojure
;; আপনার project.clj ডিপেন্ডেন্সিতে এটি যোগ করুন
[clj-yaml "0.7.0"]
```

এখানে দেখানো হলো, আপনি কিভাবে `clj-yaml` ব্যবহার করে YAML পার্স করতে এবং Clojure maps থেকে YAML-এ রূপান্তর করতে পারেন।

### YAML পার্সিং:
```clojure
(require '[clj-yaml.core :as yaml])

;; একটি YAML স্ট্রিং পার্স করা
(let [yaml-str "name: John Doe\nage: 30\nlanguages:\n  - Clojure\n  - Python"]
  (yaml/parse-string yaml-str))
;; আউটপুট:
;; => {"name" "John Doe", "age" 30, "languages" ["Clojure" "Python"]}
```

### Clojure থেকে YAML জেনারেট করা:
```clojure
(require '[clj-yaml.core :as yaml])

;; একটি Clojure map থেকে YAML স্ট্রিং-এ রূপান্তর
(let [data-map {:name "Jane Doe" :age 28 :languages ["Java" "Ruby"]}]
  (yaml/generate-string data-map))
;; আউটপুট:
; "age: 28\nlanguages:\n- Java\n- Ruby\nname: Jane Doe\n"
```

এই সহজ অপারেশনগুলি `clj-yaml` এর সাথে মিলিত করে Clojure অ্যাপ্লিকেশনগুলিতে কনফিগারেশন ফাইল হ্যান্ডল করা এবং YAML ব্যবহার করে অন্যান্য সেবা বা উপাদানের সাথে ডেটা আদানপ্রদান সুবিধাজনক করা যেতে পারে।
