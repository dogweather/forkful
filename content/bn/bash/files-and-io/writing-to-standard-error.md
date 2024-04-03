---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:42:03.398152-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09AC\u09BE\u09B6\u09C7, \u0986\
  \u0989\u099F\u09AA\u09C1\u099F\u0995\u09C7 stderr \u098F \u09B0\u09BF\u09A1\u09BE\
  \u0987\u09B0\u09C7\u0995\u09CD\u099F \u0995\u09B0\u09A4\u09C7 `>&2` \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09BE \u09B9\u09AF\u09BC\u0964 \u098F\
  \u0996\u09BE\u09A8\u09C7 \u098F\u0995\u099F\u09BF \u09AC\u09C7\u09B8\u09BF\u0995\
  \ \u0989\u09A6\u09BE\u09B9\u09B0\u09A3 \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\
  \u09B2."
lastmod: '2024-03-17T18:47:44.244382-06:00'
model: gpt-4-0125-preview
summary: "\u09AC\u09BE\u09B6\u09C7, \u0986\u0989\u099F\u09AA\u09C1\u099F\u0995\u09C7\
  \ stderr \u098F \u09B0\u09BF\u09A1\u09BE\u0987\u09B0\u09C7\u0995\u09CD\u099F \u0995\
  \u09B0\u09A4\u09C7 `>&2` \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09BE \u09B9\u09AF\u09BC\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u098F\u0995\u099F\
  \u09BF \u09AC\u09C7\u09B8\u09BF\u0995 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3 \u09A6\
  \u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2."
title: "\u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1\
  \ \u098F\u09B0\u09B0\u09C7 \u09B2\u09BF\u0996\u09A8"
weight: 25
---

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
