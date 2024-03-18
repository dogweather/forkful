---
title:                "JSON এর সাথে কাজ করা"
date:                  2024-03-17T18:29:08.340387-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি ও কেন?
Clojure এ JSON (JavaScript Object Notation) এর সাথে কাজ করা মানে JSON স্ট্রিংগুলিকে Clojure ডেটা স্ট্রাকচারগুলিতে (ম্যাপ, ভেক্টর) পার্স করা এবং উল্টোভাবে তা করা। ওয়েব সার্ভিস, API এবং অ্যাপ্লিকেশনগুলির জন্য এই কাজটি মৌলিক যা structured, টেক্সট-ভিত্তিক ফর্ম্যাটে ডেটা যোগাযোগের প্রয়োজন পূরণ করে, কারণ JSON সর্বজনীনভাবে স্বীকৃত এবং বিভিন্ন প্রোগ্রামিং পরিবেশে সমর্থিত।

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
