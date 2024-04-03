---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:29:08.340387-06:00
description: "Clojure \u098F JSON (JavaScript Object Notation) \u098F\u09B0 \u09B8\
  \u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7\
  \ JSON \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0997\u09C1\u09B2\u09BF\u0995\u09C7\
  \ Clojure \u09A1\u09C7\u099F\u09BE \u09B8\u09CD\u099F\u09CD\u09B0\u09BE\u0995\u099A\
  \u09BE\u09B0\u0997\u09C1\u09B2\u09BF\u09A4\u09C7 (\u09AE\u09CD\u09AF\u09BE\u09AA\
  , \u09AD\u09C7\u0995\u09CD\u099F\u09B0) \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\
  \u09BE \u098F\u09AC\u0982 \u0989\u09B2\u09CD\u099F\u09CB\u09AD\u09BE\u09AC\u09C7\
  \u2026"
lastmod: '2024-03-17T18:47:43.644785-06:00'
model: gpt-4-0125-preview
summary: "Clojure \u098F JSON (JavaScript Object Notation) \u098F\u09B0 \u09B8\u09BE\
  \u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 JSON\
  \ \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0997\u09C1\u09B2\u09BF\u0995\u09C7\
  \ Clojure \u09A1\u09C7\u099F\u09BE \u09B8\u09CD\u099F\u09CD\u09B0\u09BE\u0995\u099A\
  \u09BE\u09B0\u0997\u09C1\u09B2\u09BF\u09A4\u09C7 (\u09AE\u09CD\u09AF\u09BE\u09AA\
  , \u09AD\u09C7\u0995\u09CD\u099F\u09B0) \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\
  \u09BE \u098F\u09AC\u0982 \u0989\u09B2\u09CD\u099F\u09CB\u09AD\u09BE\u09AC\u09C7\
  \ \u09A4\u09BE \u0995\u09B0\u09BE\u0964 \u0993\u09AF\u09BC\u09C7\u09AC \u09B8\u09BE\
  \u09B0\u09CD\u09AD\u09BF\u09B8, API \u098F\u09AC\u0982 \u0985\u09CD\u09AF\u09BE\u09AA\
  \u09CD\u09B2\u09BF\u0995\u09C7\u09B6\u09A8\u0997\u09C1\u09B2\u09BF\u09B0 \u099C\u09A8\
  \u09CD\u09AF \u098F\u0987 \u0995\u09BE\u099C\u099F\u09BF \u09AE\u09CC\u09B2\u09BF\
  \u0995 \u09AF\u09BE structured, \u099F\u09C7\u0995\u09CD\u09B8\u099F-\u09AD\u09BF\
  \u09A4\u09CD\u09A4\u09BF\u0995 \u09AB\u09B0\u09CD\u09AE\u09CD\u09AF\u09BE\u099F\u09C7\
  \ \u09A1\u09C7\u099F\u09BE \u09AF\u09CB\u0997\u09BE\u09AF\u09CB\u0997\u09C7\u09B0\
  \ \u09AA\u09CD\u09B0\u09AF\u09BC\u09CB\u099C\u09A8 \u09AA\u09C2\u09B0\u09A3 \u0995\
  \u09B0\u09C7, \u0995\u09BE\u09B0\u09A3 JSON \u09B8\u09B0\u09CD\u09AC\u099C\u09A8\
  \u09C0\u09A8\u09AD\u09BE\u09AC\u09C7 \u09B8\u09CD\u09AC\u09C0\u0995\u09C3\u09A4\
  \ \u098F\u09AC\u0982 \u09AC\u09BF\u09AD\u09BF\u09A8\u09CD\u09A8 \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BF\u0982 \u09AA\u09B0\u09BF\u09AC\u09C7\u09B6\
  \u09C7 \u09B8\u09AE\u09B0\u09CD\u09A5\u09BF\u09A4\u0964."
title: "JSON \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\
  \u09BE"
weight: 38
---

## কিভাবে:
Clojure JSON এর সাথে কাজ করার জন্য অন্তর্নির্মিত ফাংশন অন্তর্ভুক্ত করে না, তাই আপনি সাধারণত তৃতীয় পক্ষের লাইব্রেরি ব্যবহার করবেন। `cheshire` এবং `jsonista` তাদের ব্যবহারের সহজতা এবং কর্মক্ষমতার কারণে জনপ্রিয় পছন্দ।

### Cheshire ব্যবহার করে
প্রথমে, `project.clj` এ আপনার প্রজেক্টের ডিপেন্ডেন্সিগুলিতে Cheshire যোগ করুন:
```clj
[com.fasterxml.jackson.core/jackson-core "2.12.0"]
[cheshire "5.10.1"]
```

একটি JSON স্ট্রিংকে Clojure ম্যাপে পার্স করতে এবং একটি ম্যাপকে JSON স্ট্রিংএ রূপান্তর করতে:

```clj
(require '[cheshire.core :as json])

;; JSON স্ট্রিংকে Clojure ম্যাপে পার্স করা
(let [json-input "{\"name\":\"John\", \"age\":30}"]
  (json/parse-string json-input true)) ; => {"name" "John", "age" 30}

;; Clojure ম্যাপকে JSON স্ট্রিংএ রূপান্তর করা
(let [clj-map {"name" "John", "age" 30}]
  (json/generate-string clj-map)) ; => "{\"name\":\"John\",\"age\":30}"
```

### Jsonista ব্যবহার করে
আপনার প্রজেক্টের `project.clj` এ Jsonista যোগ করুন:
```clj
[jsonista "0.3.2"]
```

Jsonista দিয়ে সমান অপারেশনগুলি:

```clj
(require '[jsonista.core :as j])

;; JSON স্ট্রিং থেকে Clojure এ পার্স করা
(let [json-input "{\"name\":\"Emily\", \"age\":25}"]
  (j/read-value json-input)) ; => {"name" "Emily", "age" 25}

;; Clojure ম্যাপ থেকে JSON স্ট্রিং তে রূপান্তর
(let [clj-map {"name" "Emily", "age" 25}]
  (j/write-value-as-string clj-map)) ; => "{\"name\":\"Emily\",\"age\":25}"
```

উভয় লাইব্রেরিতে, আপনার জটিল ডেটা স্ট্রাকচার এনকোড এবং ডিকোড করার অপশন রয়েছে, এবং সেরিয়ালাইজেশন এবং ডিসেরিয়ালাইজেশন প্রক্রিয়াগুলির অনুকূলিত করার জন্য অতিরিক্ত ফাংশন এবং প্যারামিটার রয়েছে। বেশিরভাগ অ্যাপ্লিকেশনের জন্য, দেখানো কার্যক্ষমতা Clojure অ্যাপ্লিকেশনগুলিতে JSON এর সাথে কাজ করার জন্য একটি দৃঢ় ভিত্তি প্রদান করে।
