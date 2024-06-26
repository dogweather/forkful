---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:54:48.121651-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09B0\u09C1\u09AC\u09BF\u09B0\
  \ \u09AA\u09CD\u09B0\u0995\u09BE\u09B6\u0995\u09CD\u09B7\u09AE \u09B8\u09BF\u09A8\
  \u099F\u09CD\u09AF\u09BE\u0995\u09CD\u09B8 \u09AC\u09BF\u09AD\u09BF\u09A8\u09CD\u09A8\
  \ \u09A7\u09B0\u09A8\u09C7\u09B0 \u09AB\u09BE\u0987\u09B2 \u0985\u09AA\u09BE\u09B0\
  \u09C7\u09B6\u09A8 \u09B8\u09BE\u09AE\u09B2\u09BE\u09A8\u09CB\u09B0 \u099C\u09A8\
  \u09CD\u09AF \u09B8\u0982\u0995\u09CD\u09B7\u09BF\u09AA\u09CD\u09A4 \u098F\u09AC\
  \u0982 \u09AA\u09A1\u09BC\u09BE \u09B8\u09B9\u099C \u0993\u09AF\u09BC\u09BE\u09A8\
  -\u09B2\u09BE\u0987\u09A8\u09BE\u09B0\u09CD\u09B8 \u09AA\u09CD\u09B0\u09A6\u09BE\
  \u09A8 \u0995\u09B0\u09C7\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u0995\u09BF\u099B\
  \u09C1 \u0989\u09A6\u09BE\u09B9\u09B0\u09A3 \u09A6\u09C7\u0993\u09AF\u09BC\u09BE\
  \u2026"
lastmod: '2024-03-17T18:47:44.584368-06:00'
model: gpt-4-0125-preview
summary: "\u09B0\u09C1\u09AC\u09BF\u09B0 \u09AA\u09CD\u09B0\u0995\u09BE\u09B6\u0995\
  \u09CD\u09B7\u09AE \u09B8\u09BF\u09A8\u099F\u09CD\u09AF\u09BE\u0995\u09CD\u09B8\
  \ \u09AC\u09BF\u09AD\u09BF\u09A8\u09CD\u09A8 \u09A7\u09B0\u09A8\u09C7\u09B0 \u09AB\
  \u09BE\u0987\u09B2 \u0985\u09AA\u09BE\u09B0\u09C7\u09B6\u09A8 \u09B8\u09BE\u09AE\
  \u09B2\u09BE\u09A8\u09CB\u09B0 \u099C\u09A8\u09CD\u09AF \u09B8\u0982\u0995\u09CD\
  \u09B7\u09BF\u09AA\u09CD\u09A4 \u098F\u09AC\u0982 \u09AA\u09A1\u09BC\u09BE \u09B8\
  \u09B9\u099C \u0993\u09AF\u09BC\u09BE\u09A8-\u09B2\u09BE\u0987\u09A8\u09BE\u09B0\
  \u09CD\u09B8 \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7\u0964 \u098F\
  \u0996\u09BE\u09A8\u09C7 \u0995\u09BF\u099B\u09C1 \u0989\u09A6\u09BE\u09B9\u09B0\
  \u09A3 \u09A6\u09C7\u0993\u09AF\u09BC\u09BE \u09B9\u09B2, \u09AF\u09BE \u0986\u09AA\
  \u09A8\u09BE\u09B0 \u0995\u09BE\u099C\u09C7 \u09B2\u09BE\u0997\u09A4\u09C7 \u09AA\
  \u09BE\u09B0\u09C7."
title: "CLI \u0993\u09DF\u09BE\u09A8-\u09B2\u09BE\u0987\u09A8\u09BE\u09B0\u09CD\u09B8\
  \ \u09A6\u09BF\u09DF\u09C7 \u09AB\u09BE\u0987\u09B2 \u09AE\u09CD\u09AF\u09BE\u09A8\
  \u09BF\u09AA\u09C1\u09B2\u09C7\u099F \u0995\u09B0\u09BE"
weight: 31
---

## কিভাবে:
রুবির প্রকাশক্ষম সিনট্যাক্স বিভিন্ন ধরনের ফাইল অপারেশন সামলানোর জন্য সংক্ষিপ্ত এবং পড়া সহজ ওয়ান-লাইনার্স প্রদান করে। এখানে কিছু উদাহরণ দেওয়া হল, যা আপনার কাজে লাগতে পারে:

**একটি ফাইল পড়া**

```ruby
ruby -e 'puts File.read("example.txt")'
```

এই ওয়ান-লাইনারটি 'example.txt' এর কনটেন্ট পড়ে এবং প্রিন্ট করে। সিম্পল, তবে ফাইল দ্রুত পরীক্ষা করার জন্য কার্যকর।

**একটি ফাইলে সংযোজন করা**

```ruby
ruby -e 'File.open("example.txt", "a") { |f| f.puts "New line" }'
```

কোন এডিটর খুলে না দেখে 'example.txt' তে একটি নতুন লাইন যোগ করা। লগিং বা ফাইল অন-দা-ফ্লাই আপডেট করার জন্য দারুণ।

**একটি ফাইলের নাম পরিবর্তন করা**

```ruby
ruby -e 'File.rename("example.txt", "new_example.txt")'
```

'example.txt' থেকে 'new_example.txt' এ একটি ফাইলের নাম পরিবর্তন করা। গ্রাফিক্যাল ফাইল ম্যানেজার ছাড়া নাম সংগঠিত করা বা শুধারো নাম দেওয়ার জন্য একটি দ্রুত উপায়।

**একটি ফাইল মুছে ফেলা**

```ruby
ruby -e 'File.delete("unnecessary_file.txt")'
```

আপনার যখন পরিষ্কার করে ফাইল মুছে ফেলার প্রয়োজন হয়, এই ওয়ান-লাইনারটি আপনার যেতে হবে।

যদিও এই উদাহরণগুলি CLI থেকে রুবি দ্বারা ফাইল নিয়ন্ত্রণের সহজতাকে দেখায়, অবশ্যই ফাইল অপারেশনগুলি ব্যবহারের সময় যত্ন সহকারে মোকাবেলা করতে হবে যাতে ভুলবশত ডাটা হারানোর আশঙ্কা না থাকে। ডিলিট বা ওভাররাইট মতো ধ্বংসাত্মক অপারেশন চালানোর আগে সর্বদা গুরুত্বপূর্ণ ডাটা ব্যাকআপ নিন।

## গভীরে ডাইভ
রুবি ওয়ান-লাইনার্স দ্বারা ফাইল ম্যানিপুলেশন রুবির অনন্য নয়; পার্ল এবং এউক মতো ভাষা দশকগুলো ধরে অনুরূপ কাজে ব্যবহার হয়ে আসছে। তবে, রুবি, পার্লের প্রকাশক্ষমতার সাথে পড়ার সহজতাকে মিশ্রিত করে, স্ক্রিপ্টিং আরও সহজবোধ্য করে তোলে। তবে বলা বাহুল্য, CLI ফাইল ম্যানিপুলেশনে রুবির একটি দুর্বলতা হতে পারে এর কর্মক্ষমতা, বিশেষ করে বড় ফাইল বা জটিল অপারেশন নিয়ে কাজ করার সময়—স্ক্রিপ্টিং ভাষা সাধারণত কম্পাইলড ভাষা বা `sed` বা `awk` মতো নির্দিষ্ট ইউনিক্স টুলের চেয়ে ধীর।

তবুও, রুবি স্ক্রিপ্টস অত্যন্ত বহুমুখী এবং বৃহত্তর রুবি অ্যাপ্লিকেশন বা রেলস প্রকল্পে সহজেই একীভূত করা যায়। এদের পঠনযোগ্যতা এবং মানক লাইব্রেরি এবং জেমস এর মাধ্যমে প্রদত্ত বিস্তৃত কার্যকারিতা কর্মক্ষমতা এবং উত্পাদনশীলতার মধ্যে একটি ভারসাম্য খুঁজছে ডেভেলপারদের জন্য রুবিকে একটি দৃঢ় বিকল্প করে তোলে।

ফাইল ম্যানিপুলেশনের জন্য বিকল্প হিসেবে ইউনিক্স/লিনাক্স কমান্ড, পার্ল, অথবা পাইথন ব্যবহার করা যেতে পারে। এগুলো প্রত্যেকেরই তাদের নিজস্ব শক্তি রয়েছে; যেমন, সরল কাজের জন্য ইউনিক্স কমান্ডের কর্মক্ষমতা অপ্রতিরোধ্য, পাইথন পড়ার সহজতা এবং দক্ষতার মধ্যে একটি ভারসাম্য রক্ষা করে, এবং পার্ল টেক্সট প্রসেসিংয়ের জন্য একটি শক্তিশালী সাঁজোয়া হিসেবে রয়ে গেছে। পছন্দটি প্রায়ই নিজস্ব পছন্দ, কাজের জটিলতা এবং স্ক্রিপ্টগুলি যে পরিবেশে চালানো হবে সেই উপর নির্ভর করে।

এই বিকল্পগুলি এবং প্রোগ্রামিং-এ ফাইল ম্যানিপুলেশনের ঐতিহাসিক প্রেক্ষাপট বুঝতে পারলে, আমরা আধুনিক উন্নয়নে রুবির স্থান উপলব্ধি করতে পারি, এর শক্তি এবং অন্যান্য টুল যেখানে আরও উপযুক্ত হতে পারে সেগুলির স্বীকৃতি দিতে পারি।
