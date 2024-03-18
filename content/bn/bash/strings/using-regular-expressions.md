---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:26:05.266812-06:00
description: "\u09AC\u09CD\u09AF\u09BE\u09B6\u09C7 \u09A8\u09BF\u09AF\u09BC\u09AE\u09BF\
  \u09A4 \u0985\u09AD\u09BF\u09AC\u09CD\u09AF\u0995\u09CD\u09A4\u09BF (regex) \u0986\
  \u09AA\u09A8\u09BE\u0995\u09C7 \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F\
  \ \u09AA\u09CD\u09AF\u09BE\u099F\u09BE\u09B0\u09CD\u09A8 \u09AD\u09BF\u09A4\u09CD\
  \u09A4\u09BF\u0995 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u09AC\u0982\
  \ \u09AB\u09BE\u0987\u09B2 \u0985\u09A8\u09C1\u09B8\u09A8\u09CD\u09A7\u09BE\u09A8\
  , \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8 \u098F\u09AC\u0982 \u09AA\u09B0\
  \u09BF\u099A\u09BE\u09B2\u09A8\u09BE \u0995\u09B0\u09A4\u09C7 \u09A6\u09C7\u09AF\
  \u09BC\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\
  \u09BE \u0987\u09A8\u09AA\u09C1\u099F\u2026"
lastmod: '2024-03-17T18:47:44.213525-06:00'
model: gpt-4-0125-preview
summary: "\u09AC\u09CD\u09AF\u09BE\u09B6\u09C7 \u09A8\u09BF\u09AF\u09BC\u09AE\u09BF\
  \u09A4 \u0985\u09AD\u09BF\u09AC\u09CD\u09AF\u0995\u09CD\u09A4\u09BF (regex) \u0986\
  \u09AA\u09A8\u09BE\u0995\u09C7 \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F\
  \ \u09AA\u09CD\u09AF\u09BE\u099F\u09BE\u09B0\u09CD\u09A8 \u09AD\u09BF\u09A4\u09CD\
  \u09A4\u09BF\u0995 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u098F\u09AC\u0982\
  \ \u09AB\u09BE\u0987\u09B2 \u0985\u09A8\u09C1\u09B8\u09A8\u09CD\u09A7\u09BE\u09A8\
  , \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8 \u098F\u09AC\u0982 \u09AA\u09B0\
  \u09BF\u099A\u09BE\u09B2\u09A8\u09BE \u0995\u09B0\u09A4\u09C7 \u09A6\u09C7\u09AF\
  \u09BC\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\
  \u09BE \u0987\u09A8\u09AA\u09C1\u099F\u2026"
title: "\u09B0\u09C7\u0997\u09C1\u09B2\u09BE\u09B0 \u098F\u0995\u09CD\u09B8\u09AA\u09CD\
  \u09B0\u09C7\u09B6\u09A8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09BE"
---

{{< edit_this_page >}}

## কি ও কেন?

ব্যাশে নিয়মিত অভিব্যক্তি (regex) আপনাকে নির্দিষ্ট প্যাটার্ন ভিত্তিক স্ট্রিং এবং ফাইল অনুসন্ধান, পরিবর্তন এবং পরিচালনা করতে দেয়। প্রোগ্রামাররা ইনপুট যাচাই, লগ ফাইল পার্সিং এবং ডেটা এক্সট্রাকশনের মতো কাজের জন্য regex ব্যবহার করে কারণ এটি জটিল টেক্সট প্রক্রিয়াজাত প্রয়োজনের জন্য প্যাটার্ন নির্দিষ্ট করার একটি নমনীয় ও শক্তিশালী উপায় সরবরাহ করে।

## কিভাবে:

### মৌলিক প্যাটার্ন ম্যাচিং
কোন স্ট্রিং কোন প্যাটার্নের সাথে মিলে কিনা খুঁজে বের করতে, আপনি `grep` ব্যবহার করতে পারেন, যা একটি কমান্ড-লাইন ইউটিলিটি যা প্লেইন-টেক্সট ডেটা সেটে নির্দিষ্ট নিয়মিত অভিব্যক্তি মেলে এমন লাইনগুলিকে অনুসন্ধান করে:

```bash
echo "Hello, World!" | grep -o "World"
# আউটপুট: World
```

### নির্দিষ্ট ডেটা এক্সট্রাক্ট করা
আপনার regex প্যাটার্ন মেলে এমন ডেটার অংশ এক্সট্রাক্ট করতে, আপনি `grep` এর সাথে `-o` ব্যবহার করতে পারেন:

```bash
echo "Error: File not found" | grep -oE "[A-Za-z]+:"
# আউটপুট: Error:
```

### `sed` এর সাথে Regex ব্যবহার করা
`sed` (স্ট্রিম এডিটর) টেক্সট পার্স এবং রূপান্তর করার জন্য একটি শক্তিশালী ইউটিলিটি। `sed` এর সাথে regex ব্যবহার করে টেক্সট প্রতিস্থাপন করার পদ্ধতি এখানে:

```bash
echo "Bash is great" | sed -e 's/great/awesome/'
# আউটপুট: Bash is awesome
```

### শর্তাধীন বিবৃতিতে প্যাটার্ন ম্যাচিং
ব্যাশ সরাসরি শর্তাধীন বিবৃতিতেও regex সমর্থন করে:

```bash
[[ "https://example.com" =~ ^https?:// ]] && echo "URL is valid" || echo "URL is invalid"
# আউটপুট: URL is valid
```

### `awk` এর সাথে উন্নত প্যাটার্ন ম্যাচিং এবং ম্যানিপুলেশন
`awk` একটি আরেকটি টেক্সট-প্রক্রিয়াকরণ টুল যা আরও জটিল ডেটা এক্সট্রাকশন এবং ম্যানিপুলেশন সমর্থন করে। এটি CSV এর মতো গঠিত টেক্সট ডেটা নিয়ে কাজ করার সময় সুবিধাজনক হতে পারে:

```bash
echo -e "ID,Name,Age\n1,John,22\n2,Jane,24" | awk -F, '$3 > 22 {print $2 " is older than 22."}'
# আউটপুট: Jane is older than 22.
```

ব্যাশের অন্তর্মুখী regex কার্যক্ষমতা অনেক ব্যবহার কেস কভার করে, খুব উন্নত regex অপারেশনগুলির জন্য, আপনি ব্যাশ স্ক্রিপ্টসের সাথে `perl` অথবা `python` স্ক্রিপ্টসের সমন্বয় বিবেচনা করতে পারেন, যেহেতু এই ভাষাগুলি শক্তিশালী regex লাইব্রেরি (যেমন, Python এ `re`) অফার করে। Python এর সাথে একটি সহজ উদাহরণ:

```bash
echo "Capture this 123" | python3 -c "import sys; import re; print(re.search('(\d+)', sys.stdin.read()).group(0))"
# আউটপুট: 123
```

প্রয়োজনে এই প্রোগ্রামিং ভাষাগুলি অন্তর্ভুক্ত করে, আপনার ব্যাশ স্ক্রিপ্টগুলিতে regex এর পূর্ণ শক্তি কাজে লাগাতে সাহায্য করতে পারে।
