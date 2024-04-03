---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:18:47.541359-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u099A\u09B2\u09C1\u09A8 \u0995\
  \u09CB\u09A1\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BF\u099B\u09C1 \u0995\
  \u09BE\u099C \u0995\u09B0\u09BF\u0964 \u0986\u09AE\u09B0\u09BE `curl`, \u098F\u0995\
  \u099F\u09BF \u09AA\u09CD\u09B0\u099A\u09B2\u09BF\u09A4 \u0995\u09AE\u09BE\u09A8\
  \u09CD\u09A1-\u09B2\u09BE\u0987\u09A8 \u099F\u09C1\u09B2 \u09AC\u09CD\u09AF\u09AC\
  \u09B9\u09BE\u09B0 \u0995\u09B0\u09AC\u0964 `username:password` \u0986\u09AA\u09A8\
  \u09BE\u09B0 \u09AA\u09CD\u09B0\u09AE\u09BE\u09A3\u09C0\u0995\u09B0\u09A3 \u09A4\
  \u09A5\u09CD\u09AF \u09A6\u09BF\u09AF\u09BC\u09C7 \u098F\u09AC\u0982\u2026"
lastmod: '2024-03-17T18:47:44.226389-06:00'
model: gpt-4-0125-preview
summary: "\u099A\u09B2\u09C1\u09A8 \u0995\u09CB\u09A1\u09C7\u09B0 \u09B8\u09BE\u09A5\
  \u09C7 \u0995\u09BF\u099B\u09C1 \u0995\u09BE\u099C \u0995\u09B0\u09BF\u0964 \u0986\
  \u09AE\u09B0\u09BE `curl`, \u098F\u0995\u099F\u09BF \u09AA\u09CD\u09B0\u099A\u09B2\
  \u09BF\u09A4 \u0995\u09AE\u09BE\u09A8\u09CD\u09A1-\u09B2\u09BE\u0987\u09A8 \u099F\
  \u09C1\u09B2 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09AC\u0964\
  \ `username:password` \u0986\u09AA\u09A8\u09BE\u09B0 \u09AA\u09CD\u09B0\u09AE\u09BE\
  \u09A3\u09C0\u0995\u09B0\u09A3 \u09A4\u09A5\u09CD\u09AF \u09A6\u09BF\u09AF\u09BC\
  \u09C7 \u098F\u09AC\u0982 `http://example.com/resource` \u0986\u09AA\u09A8\u09BE\
  \u09B0 \u09B2\u0995\u09CD\u09B7\u09CD\u09AF URL \u09A6\u09BF\u09AF\u09BC\u09C7 \u09AA\
  \u09CD\u09B0\u09A4\u09BF\u09B8\u09CD\u09A5\u09BE\u09AA\u09A8 \u0995\u09B0\u09C1\u09A8\
  \u0964."
title: "\u09AC\u09C7\u09B8\u09BF\u0995 \u0985\u09A5\u09C7\u09A8\u09CD\u099F\u09BF\u0995\
  \u09C7\u09B6\u09A8 \u09B8\u09B9 HTTP \u09B0\u09BF\u0995\u09C1\u09DF\u09C7\u09B8\u09CD\
  \u099F \u09AA\u09CD\u09B0\u09C7\u09B0\u09A3"
weight: 45
---

## কিভাবে:
চলুন কোডের সাথে কিছু কাজ করি। আমরা `curl`, একটি প্রচলিত কমান্ড-লাইন টুল ব্যবহার করব। `username:password` আপনার প্রমাণীকরণ তথ্য দিয়ে এবং `http://example.com/resource` আপনার লক্ষ্য URL দিয়ে প্রতিস্থাপন করুন।

```Bash
curl -u username:password http://example.com/resource
```

অথবা আপনার প্রমাণীকরণ তথ্যগুলি পূর্বে base64-এ কোড করুন এবং তাদের নিম্নরূপ ব্যবহার করুন:

```Bash
# প্রমাণীকরণ তথ্যগুলি কোড করা
credentials=$(echo -n username:password | base64)

# অনুরোধ প্রেরণ করা
curl -H "Authorization: Basic $credentials" http://example.com/resource
```

একটি সফল অনুরোধের নমুনা আউটপুট এরকম দেখাবে:

```Bash
{
  "data": "কিছু সীমাবদ্ধ তথ্য",
  "message": "প্রবেশাধিকার অনুমোদিত"
}
```

## গভীরে ডুব:
ঐতিহাসিকভাবে, মৌলিক প্রমাণীকরণ HTTP-এর প্রাথমিক দিন থেকে একটি অংশ হয়েছে, তবে এটি দোষযুক্ত - মূলত HTTPS মতো নিরাপদ চ্যানেলের উপর ব্যবহার না হলে এর দুর্বলতা।

বিকল্পগুলির মধ্যে OAuth রয়েছে, যা আরও নিরাপদ এবং অ্যাক্সেসের উপর আরও সূক্ষ্ম নিয়ন্ত্রণ প্রদান করে। Digest প্রমাণীকরণ আরেকটি, সাদা পাঠ্যের পরিবর্তে হ্যাশ করা প্রমাণীকরণ পাঠায়।

যান্ত্রিকী ব্যাপারগুলোর জন্য, যখন আপনি মৌলিক প্রমাণীকরণ প্রমাণীকরণ পাঠান, তারা HTTP হেডারে Base64-এ কোডেড অবস্থায় অন্তর্ভুক্ত করা হয়। এটি এনক্রিপশন নয়, তাই যদি আপনি HTTPS ব্যবহার না করেন, অনুরোধটি যে কেউ আঁচ করতে পারে তাদের দ্বারা সহজেই ডিকোড করা যাবে। HTTPS ব্যবহার করা ট্রান্সমিশনকে নিরাপদ করে, ক্লায়েন্ট এবং সার্ভারের মধ্যে সবকিছু এনক্রিপ্ট করে।

## আরও দেখুন
- cURL অফিসিয়াল ডকুমেন্টেশন: https://curl.haxx.se/docs/manpage.html
- HTTP প্রমাণীকরণ: মৌলিক এবং ডাইজেস্ট এক্সেস প্রমাণীকরণ (IETF RFC 7617): https://tools.ietf.org/html/rfc7617
- OAuth পরিচিতি: https://oauth.net/2/introduction/
