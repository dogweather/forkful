---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:28:50.237575-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Bash \u098F\u09B0 \u09A8\u09BF\
  \u099C\u09B8\u09CD\u09AC JSON \u09AA\u09BE\u09B0\u09CD\u09B8\u09BF\u0982 \u0995\u09CD\
  \u09B7\u09AE\u09A4\u09BE \u09A8\u09C7\u0987, \u0995\u09BF\u09A8\u09CD\u09A4\u09C1\
  \ `jq` \u09B9\u09B2 \u098F\u0995\u099F\u09BF \u09B6\u0995\u09CD\u09A4\u09BF\u09B6\
  \u09BE\u09B2\u09C0 \u0995\u09AE\u09BE\u09A8\u09CD\u09A1-\u09B2\u09BE\u0987\u09A8\
  \ JSON \u09AA\u09CD\u09B0\u09B8\u09C7\u09B8\u09B0 \u09AF\u09BE \u098F\u0987 \u0998\
  \u09BE\u099F\u09A4\u09BF \u09AA\u09C2\u09B0\u09A3 \u0995\u09B0\u09C7\u0964 \u098F\
  \u09B0 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u09A8\u09BF\u09AE\u09CD\u09A8\
  \u09B0\u09C2\u09AA: **\u098F\u0995\u099F\u09BF\u2026"
lastmod: '2024-03-17T18:47:44.249483-06:00'
model: gpt-4-0125-preview
summary: "Bash \u098F\u09B0 \u09A8\u09BF\u099C\u09B8\u09CD\u09AC JSON \u09AA\u09BE\
  \u09B0\u09CD\u09B8\u09BF\u0982 \u0995\u09CD\u09B7\u09AE\u09A4\u09BE \u09A8\u09C7\
  \u0987, \u0995\u09BF\u09A8\u09CD\u09A4\u09C1 `jq` \u09B9\u09B2 \u098F\u0995\u099F\
  \u09BF \u09B6\u0995\u09CD\u09A4\u09BF\u09B6\u09BE\u09B2\u09C0 \u0995\u09AE\u09BE\
  \u09A8\u09CD\u09A1-\u09B2\u09BE\u0987\u09A8 JSON \u09AA\u09CD\u09B0\u09B8\u09C7\u09B8\
  \u09B0 \u09AF\u09BE \u098F\u0987 \u0998\u09BE\u099F\u09A4\u09BF \u09AA\u09C2\u09B0\
  \u09A3 \u0995\u09B0\u09C7\u0964 \u098F\u09B0 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u09A8\u09BF\u09AE\u09CD\u09A8\u09B0\u09C2\u09AA."
title: "JSON \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\
  \u09BE"
weight: 38
---

## কিভাবে:
Bash এর নিজস্ব JSON পার্সিং ক্ষমতা নেই, কিন্তু `jq` হল একটি শক্তিশালী কমান্ড-লাইন JSON প্রসেসর যা এই ঘাটতি পূরণ করে। এর ব্যবহার নিম্নরূপ:

**একটি JSON ফাইল পড়া:**

উদাহরণ `data.json`:
```json
{
  "name": "Jane Doe",
  "email": "jane@example.com",
  "location": {
    "city": "New York",
    "country": "USA"
  }
}
```

JSON ফাইল থেকে নাম পড়া এবং এক্সট্র্যাক্ট করা:
```bash
jq '.name' data.json
```
আউটপুট:
```
"Jane Doe"
```

**JSON ডেটা মডিফাই করা:**

সিটিটি "Los Angeles" এ আপডেট করে ফাইলে লিখে ফেলা:
```bash
jq '.location.city = "Los Angeles"' data.json > temp.json && mv temp.json data.json
```

**ভেরিয়েবল থেকে JSON পার্স করা:**

যদি আপনার কাছে Bash ভেরিয়েবলে JSON থাকে, `jq` এটি প্রক্রিয়া করতে পারে:
```bash
json_string='{"name": "John Doe", "email": "john@example.com"}'
echo $json_string | jq '.name'
```
আউটপুট:
```
"John Doe"
```

**অ্যারে নিয়ে কাজ করা:**

JSON এ একটি আইটেমের অ্যারে দেওয়া আছে:
```json
{
  "items": ["apple", "banana", "cherry"]
}
```

দ্বিতীয় আইটেমটি (ইনডেক্সিং 0 থেকে শুরু) এক্সট্র্যাক্ট করা:
```bash
jq '.items[1]' data.json
```
আউটপুট:
```
"banana"
```

আরো জটিল অপারেশন এবং ফিল্টারিং এর জন্য, `jq` এর একটি বিস্তারিত ম্যানুয়াল এবং টিউটোরিয়াল অনলাইনে উপলব্ধ, যা এটিকে আপনার সব Bash/JSON চাহিদাগুলির জন্য একটি বহুমুখী টুল করে তোলে।
