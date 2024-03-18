---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:42:03.398152-06:00
description: "\u09AC\u09BE\u09B6\u09C7 \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\
  \u09A1\u09BE\u09B0\u09CD\u09A1 \u098F\u09B0\u09B0 (stderr) \u098F \u09B2\u09BF\u0996\
  \u09BE \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2 \u098F\u09B0\u09B0 \u09AE\u09C7\u09B8\
  \u09C7\u099C\u0997\u09C1\u09B2\u09CB \u09AC\u09BE \u09AF\u09C7\u0995\u09CB\u09A8\
  \u09CB \u0997\u09C1\u09B0\u09C1\u09A4\u09CD\u09AC\u09AA\u09C2\u09B0\u09CD\u09A3\
  \ \u09A1\u09BE\u09AF\u09BC\u09BE\u0997\u09A8\u09B8\u09CD\u099F\u09BF\u0995 \u0986\
  \u0989\u099F\u09AA\u09C1\u099F\u0995\u09C7 \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\
  \u09CD\u09A1\u09BE\u09B0\u09CD\u09A1 \u0986\u0989\u099F\u09AA\u09C1\u099F (stdout)\
  \ \u09A5\u09C7\u0995\u09C7 \u09AA\u09C3\u09A5\u0995 \u0995\u09B0\u09C7\u2026"
lastmod: '2024-03-17T18:47:44.244382-06:00'
model: gpt-4-0125-preview
summary: "\u09AC\u09BE\u09B6\u09C7 \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\
  \u09A1\u09BE\u09B0\u09CD\u09A1 \u098F\u09B0\u09B0 (stderr) \u098F \u09B2\u09BF\u0996\
  \u09BE \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2 \u098F\u09B0\u09B0 \u09AE\u09C7\u09B8\
  \u09C7\u099C\u0997\u09C1\u09B2\u09CB \u09AC\u09BE \u09AF\u09C7\u0995\u09CB\u09A8\
  \u09CB \u0997\u09C1\u09B0\u09C1\u09A4\u09CD\u09AC\u09AA\u09C2\u09B0\u09CD\u09A3\
  \ \u09A1\u09BE\u09AF\u09BC\u09BE\u0997\u09A8\u09B8\u09CD\u099F\u09BF\u0995 \u0986\
  \u0989\u099F\u09AA\u09C1\u099F\u0995\u09C7 \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\
  \u09CD\u09A1\u09BE\u09B0\u09CD\u09A1 \u0986\u0989\u099F\u09AA\u09C1\u099F (stdout)\
  \ \u09A5\u09C7\u0995\u09C7 \u09AA\u09C3\u09A5\u0995 \u0995\u09B0\u09C7\u2026"
title: "\u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1\
  \ \u098F\u09B0\u09B0\u09C7 \u09B2\u09BF\u0996\u09A8"
---

{{< edit_this_page >}}

## কি এবং কেন?
বাশে স্ট্যান্ডার্ড এরর (stderr) এ লিখা মানে হল এরর মেসেজগুলো বা যেকোনো গুরুত্বপূর্ণ ডায়াগনস্টিক আউটপুটকে স্ট্যান্ডার্ড আউটপুট (stdout) থেকে পৃথক করে দেওয়া। প্রোগ্রামাররা এটি করেন এই নিশ্চিত করতে যে এরর মেসেজগুলো সহজে চিহ্নিত, লগ করা বা এমনকি উপেক্ষা করা যায়, যা ডিবাগিং এবং লগিং প্রক্রিয়াগুলিকে সহায়তা করে।

## কিভাবে:
বাশে, আউটপুটকে stderr এ রিডাইরেক্ট করতে `>&2` ব্যবহার করা হয়। এখানে একটি বেসিক উদাহরণ দেওয়া হল:

```bash
echo "This is a normal message"
echo "This is an error message" >&2
```

এই স্ক্রিপ্ট চালানো হলে, কনসোলে উভয় মেসেজই দেখানো হবে, কিন্তু যদি আপনি তাদের রিডাইরেক্ট করেন, তাহলে আপনি stdout থেকে stderr কে পৃথক করতে পারবেন। উদাহরণস্বরূপ:

```bash
bash script.sh > output.txt 2> error.txt
```

`output.txt` তে `"This is a normal message"` থাকবে, অন্যদিকে `error.txt` ক্যাপচার করবে `"This is an error message"`।

একটি বাস্তবিক ব্যবহারের ক্ষেত্রে, ধরুন একটি স্ক্রিপ্ট যা ফাইলগুলি প্রসেস করে এবং যদি কোন ফাইল না থাকে তবে একটি এরর রিপোর্ট করে:

```bash
filename="example.txt"

if [ ! -f "$filename" ]; then
    echo "$filename does not exist!" >&2
    exit 1
else
    echo "Processing $filename"
fi
```

`example.txt` না থাকার সময় কনসোলে সরাসরি সাম্পল আউটপুট:

```
example.txt does not exist!
```

বাশে stderr হ্যান্ডেল করার জন্য কোনো সরাসরি তৃতীয়-পক্ষের লাইব্রেরি নেই, যেহেতু রিডাইরেকশন স্বাভাবিকভাবে সমর্থিত এবং সাধারণত যথেষ্ট। তবে, জটিল অ্যাপ্লিকেশনগুলির জন্য, লগিং ফ্রেমওয়ার্ক অথবা বাইরের লগিং টুলস যেমন `syslog` অথবা `log4bash` উভয় stdout এবং stderr কে আরও কার্যকরীভাবে ম্যানেজ করার জন্য অন্তর্ভুক্ত করা যেতে পারে।
