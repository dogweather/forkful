---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:29:08.340387-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Clojure JSON \u098F\u09B0 \u09B8\
  \u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\
  \u09AF \u0985\u09A8\u09CD\u09A4\u09B0\u09CD\u09A8\u09BF\u09B0\u09CD\u09AE\u09BF\u09A4\
  \ \u09AB\u09BE\u0982\u09B6\u09A8 \u0985\u09A8\u09CD\u09A4\u09B0\u09CD\u09AD\u09C1\
  \u0995\u09CD\u09A4 \u0995\u09B0\u09C7 \u09A8\u09BE, \u09A4\u09BE\u0987 \u0986\u09AA\
  \u09A8\u09BF \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3\u09A4 \u09A4\u09C3\u09A4\u09C0\
  \u09AF\u09BC \u09AA\u0995\u09CD\u09B7\u09C7\u09B0 \u09B2\u09BE\u0987\u09AC\u09CD\
  \u09B0\u09C7\u09B0\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09AC\u09C7\u09A8\u0964 `cheshire` \u098F\u09AC\u0982\u2026"
lastmod: '2024-03-17T18:47:43.644785-06:00'
model: gpt-4-0125-preview
summary: "Clojure JSON \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\
  \u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u0985\u09A8\u09CD\u09A4\u09B0\u09CD\
  \u09A8\u09BF\u09B0\u09CD\u09AE\u09BF\u09A4 \u09AB\u09BE\u0982\u09B6\u09A8 \u0985\
  \u09A8\u09CD\u09A4\u09B0\u09CD\u09AD\u09C1\u0995\u09CD\u09A4 \u0995\u09B0\u09C7\
  \ \u09A8\u09BE, \u09A4\u09BE\u0987 \u0986\u09AA\u09A8\u09BF \u09B8\u09BE\u09A7\u09BE\
  \u09B0\u09A3\u09A4 \u09A4\u09C3\u09A4\u09C0\u09AF\u09BC \u09AA\u0995\u09CD\u09B7\
  \u09C7\u09B0 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09AC\u09C7\u09A8\u0964 `cheshire` \u098F\
  \u09AC\u0982 `jsonista` \u09A4\u09BE\u09A6\u09C7\u09B0 \u09AC\u09CD\u09AF\u09AC\u09B9\
  \u09BE\u09B0\u09C7\u09B0 \u09B8\u09B9\u099C\u09A4\u09BE \u098F\u09AC\u0982 \u0995\
  \u09B0\u09CD\u09AE\u0995\u09CD\u09B7\u09AE\u09A4\u09BE\u09B0 \u0995\u09BE\u09B0\u09A3\
  \u09C7 \u099C\u09A8\u09AA\u09CD\u09B0\u09BF\u09AF\u09BC \u09AA\u099B\u09A8\u09CD\
  \u09A6\u0964\n\n#."
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
