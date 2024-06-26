---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:54:43.980436-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09AB\u09BF\u09B6 \u09B6\u09C7\
  \u09B2\u09C7 \u09AB\u09BE\u0987\u09B2 \u09AE\u09CD\u09AF\u09BE\u09A8\u09BF\u09AA\
  \u09C1\u09B2\u09C7\u099F \u0995\u09B0\u09BE \u098F\u0995\u0987 \u09B8\u09BE\u09A5\
  \u09C7 \u09B8\u09B9\u099C\u09BE\u09A4 \u098F\u09AC\u0982 \u09B6\u0995\u09CD\u09A4\
  \u09BF\u09B6\u09BE\u09B2\u09C0\u0964 \u098F\u09B0 \u0995\u09CD\u09B7\u09AE\u09A4\
  \u09BE \u09AA\u09CD\u09B0\u09A6\u09B0\u09CD\u09B6\u09A8\u09C7\u09B0 \u099C\u09A8\
  \u09CD\u09AF \u098F\u0996\u09BE\u09A8\u09C7 \u0995\u09BF\u099B\u09C1 \u0989\u09A6\
  \u09BE\u09B9\u09B0\u09A3 \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2\u09CB\
  : \u09E7. **\u09AB\u09BE\u0987\u09B2 \u09A4\u09C8\u09B0\u09BF** \u0995\u09B0\u09BE\
  \ \u098F\u0995\u09C7\u09AC\u09BE\u09B0\u09C7\u2026"
lastmod: '2024-03-17T18:47:44.492283-06:00'
model: gpt-4-0125-preview
summary: "\u09AB\u09BF\u09B6 \u09B6\u09C7\u09B2\u09C7 \u09AB\u09BE\u0987\u09B2 \u09AE\
  \u09CD\u09AF\u09BE\u09A8\u09BF\u09AA\u09C1\u09B2\u09C7\u099F \u0995\u09B0\u09BE\
  \ \u098F\u0995\u0987 \u09B8\u09BE\u09A5\u09C7 \u09B8\u09B9\u099C\u09BE\u09A4 \u098F\
  \u09AC\u0982 \u09B6\u0995\u09CD\u09A4\u09BF\u09B6\u09BE\u09B2\u09C0\u0964 \u098F\
  \u09B0 \u0995\u09CD\u09B7\u09AE\u09A4\u09BE \u09AA\u09CD\u09B0\u09A6\u09B0\u09CD\
  \u09B6\u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u0996\u09BE\u09A8\u09C7\
  \ \u0995\u09BF\u099B\u09C1 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3 \u09A6\u09C7\u0993\
  \u09AF\u09BC\u09BE \u09B9\u09B2\u09CB."
title: "CLI \u0993\u09DF\u09BE\u09A8-\u09B2\u09BE\u0987\u09A8\u09BE\u09B0\u09CD\u09B8\
  \ \u09A6\u09BF\u09DF\u09C7 \u09AB\u09BE\u0987\u09B2 \u09AE\u09CD\u09AF\u09BE\u09A8\
  \u09BF\u09AA\u09C1\u09B2\u09C7\u099F \u0995\u09B0\u09BE"
weight: 31
---

## কিভাবে:
ফিশ শেলে ফাইল ম্যানিপুলেট করা একই সাথে সহজাত এবং শক্তিশালী। এর ক্ষমতা প্রদর্শনের জন্য এখানে কিছু উদাহরণ দেওয়া হলো:

১. **ফাইল তৈরি** করা একেবারে সোজা। `touch` কমান্ড ব্যবহার করুন:

```Fish Shell
touch myfile.txt
```

এই কমান্ডটি `myfile.txt` নামে একটি খালি ফাইল তৈরি করে।

২. **ফাইলে টেক্সট লেখা** এটি `echo` কমান্ডের সাথে রিডিরেকশন অপারেটর ব্যবহার করে করা যায়:

```Fish Shell
echo "Hello, Fish Shell!" > hello.txt
```

এটি "Hello, Fish Shell!" টেক্সটটি `hello.txt` ফাইলে লিখবে, এর কন্টেন্টস ওভাররাইট করে।

৩. **ফাইলে টেক্সট যোগ করা** পূর্ববর্তী কন্টেন্ট মুছে ফেলা ছাড়া `>>` ব্যবহার করে হয়:

```Fish Shell
echo "Another line." >> hello.txt
```

এখন `hello.txt` ফাইলে দুটি লাইনের টেক্সট আছে।

৪. **ফাইলের কন্টেন্ট পড়া** `cat` দিয়ে সহজ:

```Fish Shell
cat hello.txt
```

আউটপুট:
```
Hello, Fish Shell!
Another line.
```

৫. **ফাইল খুঁজে পেতে** `find` কমান্ড শক্তিশালী সার্চ প্যাটার্ন অনুমতি দেয়। বর্তমান ডিরেক্টরি এবং সাবডিরেক্টরিতে সকল `.txt` ফাইল খুঁজে পেতে:

```Fish Shell
find . -type f -name "*.txt"
```

৬. **বাল্ক রিনেমিং** একটি লুপের সাথে সুন্দরভাবে হ্যান্ডেল করা যায়। এখানে সকল `.txt` ফাইলের শুরুতে `new_` যোগ করার জন্য একটি সহজ স্নিপেট:

```Fish Shell
for file in *.txt
    mv $file "new_$file"
end
```

৭. **ফাইল রিমুভ করা** `rm` দিয়ে করা হয়। প্রতিটি ডিলিশনের আগে একটি প্রম্পট সহ সকল `.txt` ফাইল নিরাপদে অপসারণের জন্য:

```Fish Shell
for file in *.txt
    rm -i $file
end
```

## গভীর ডাইভ
CLI দিয়ে ফিশ শেলের একলাইনারদ্বারা ফাইল ম্যানিপুলেট করা একটি দক্ষতা এবং শিল্প উভয়ই। ঐতিহাসিকভাবে, ইউনিক্স এবং লিনাক্স সিস্টেমের সবসময়ই ফাইল ম্যানিপ্যুলেশনের জন্য একটি শক্তিশালী টুলসেট অফার করা হয়েছিল, এর দর্শনে সবকিছুকে একটি ফাইল হিসেবে বিবেচনা করা হয়। এটি ফিশের মতো আধুনিক শেলের জন্য পথ প্রশস্ত করেছে, যা শুধু এই দর্শনগুলি গ্রহণ করে না বরং উন্নত সিনট্যাক্স এবং অতিরিক্ত ইউটিলিটিজ সহ এগুলি বিস্তারিত করে।

যদিও ফিশ অসামান্য ব্যবহারকারী অভিজ্ঞতা এবং স্ক্রিপ্টিং ক্ষমতা সরবরাহ করে, POSIX অনুবর্তিতা সম্পর্কিত ইস্যু তুলে ধরা মূল্যবান, বিশেষ করে যখন স্ক্রিপ্টগুলি ব্যাশ বা SH এর মতো আরও প্রথাগত শেল থেকে পোর্ট করা হয়। এর কারণ হলো ফিশ ডিজাইন দ্বারা POSIX-সম্মত হতে লক্ষ্য করেনি, বরং স্ক্রিপ্টিং এবং কমান্ড-লাইন ব্যবহারে একটি আরও ব্যবহারকারী-বান্ধব প্রক্রিয়া গ্রহণ করে। অতএব, প্রোগ্রামারদের সচেতন থাকা উচিত যে, যদিও ফিশ অনেক ক্ষেত্রে সফল, POSIX অনুবর্তন সম্পর্কিত স্ক্রিপ্টগুলির জন্�
