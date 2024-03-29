---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:25:57.947701-06:00
description: "\u09AA\u09BE\u0987\u09A5\u09A8\u09C7 \u09AF\u09BE associative arrays\
  \ \u09B9\u09BF\u09B8\u09C7\u09AC\u09C7 \u09AA\u09B0\u09BF\u099A\u09BF\u09A4, \u09A4\
  \u09BE dictionaries \u09A8\u09BE\u09AE\u09C7 \u099C\u09BE\u09A8\u09BE \u09AF\u09BE\
  \u09DF, \u09AF\u09BE \u099A\u09BE\u09AC\u09BF\u0995\u09C7 \u09AE\u09BE\u09A8\u09C7\
  \u09B0 \u09B8\u09BE\u09A5\u09C7 \u09AE\u09CD\u09AF\u09BE\u09AA \u0995\u09B0\u09C7\
  , \u098F\u09A4\u09C7 \u0995\u09B0\u09C7 \u098F\u0995\u099F\u09BF \u0985\u09A8\u09A8\
  \u09CD\u09AF \u09AA\u09B0\u09BF\u099A\u09BF\u09A4\u09BF\u09B0 \u09AE\u09BE\u09A7\
  \u09CD\u09AF\u09AE\u09C7 \u09A1\u09BE\u099F\u09BE\u2026"
lastmod: '2024-03-17T18:47:43.564498-06:00'
model: gpt-4-0125-preview
summary: "\u09AA\u09BE\u0987\u09A5\u09A8\u09C7 \u09AF\u09BE associative arrays \u09B9\
  \u09BF\u09B8\u09C7\u09AC\u09C7 \u09AA\u09B0\u09BF\u099A\u09BF\u09A4, \u09A4\u09BE\
  \ dictionaries \u09A8\u09BE\u09AE\u09C7 \u099C\u09BE\u09A8\u09BE \u09AF\u09BE\u09DF\
  , \u09AF\u09BE \u099A\u09BE\u09AC\u09BF\u0995\u09C7 \u09AE\u09BE\u09A8\u09C7\u09B0\
  \ \u09B8\u09BE\u09A5\u09C7 \u09AE\u09CD\u09AF\u09BE\u09AA \u0995\u09B0\u09C7, \u098F\
  \u09A4\u09C7 \u0995\u09B0\u09C7 \u098F\u0995\u099F\u09BF \u0985\u09A8\u09A8\u09CD\
  \u09AF \u09AA\u09B0\u09BF\u099A\u09BF\u09A4\u09BF\u09B0 \u09AE\u09BE\u09A7\u09CD\
  \u09AF\u09AE\u09C7 \u09A1\u09BE\u099F\u09BE\u2026"
title: "\u098F\u09B8\u09CB\u09B8\u09BF\u09AF\u09BC\u09C7\u099F\u09BF\u09AD \u0985\u09CD\
  \u09AF\u09BE\u09B0\u09C7\u09B0 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0"
---

{{< edit_this_page >}}

## কি এবং কেন?

পাইথনে যা associative arrays হিসেবে পরিচিত, তা dictionaries নামে জানা যায়, যা চাবিকে মানের সাথে ম্যাপ করে, এতে করে একটি অনন্য পরিচিতির মাধ্যমে ডাটা পুনঃপ্রাপ্তি, পরিবর্তন বা ট্র্যাকিং সহজ হয়। প্রোগ্রামাররা এটি উপাদানের প্রবেশে দক্ষতা এবং জটিল ডাটা কাঠামোগুলি প্রতিনিধিত্বে এর সংশ্লেষণশীলতার জন্য ব্যবহার করে থাকে।

## কিভাবে:

পাইথনে একটি ডিকশনারি তৈরি করা সোজা। আপনি কী-মানের জোড়গুলি কোষ্ঠক `{}` এ উল্লিখিত করেন, এবং চাবি এবং মানগুলি কোলনের দ্বারা পৃথক থাকে:

```Python
# একটি associative array (dictionary) তৈরি করা
my_dict = {"name": "John", "age": 30, "city": "New York"}
print(my_dict)
```

আউটপুট:
```
{'name': 'John', 'age': 30, 'city': 'New York'}
```

একটি চাবি দ্বারা মান অ্যাক্সেস করা সহজ:

```Python
# একটি মান অ্যাক্সেস করা
print(my_dict["name"])
```

আউটপুট:
```
John
```

উপাদান যোগ বা আপডেট করা একটি চাবি একটি মান বরাদ্দ করে করা হয়:

```Python
# একটি নতুন কী-মানের জোড় যোগ
my_dict["email"] = "john@example.com"
# একটি মান আপডেট করা
my_dict["age"] = 31
print(my_dict)
```

আউটপুট:
```
{'name': 'John', 'age': 31, 'city': 'New York', 'email': 'john@example.com'}
```

ডিকশনারি আইটেমগুলির উপর ইটারেট করা:

```Python
# কী-মানের জোড়ের মধ্যে ইটারেট করা
for key, value in my_dict.items():
    print(f"{key}: {value}")
```

আউটপুট:
```
name: John
age: 31
city: New York
email: john@example.com
```

## গভীর ডাইভ

পাইথনে associative arrays বা dictionaries, দক্ষ ডাটা প্রবেশ এবং ম্যানিপুলেশনের জন্য একটি ডাটা কাঠামো হিসেবে পরিচিতি পেয়েছে। ধারাবাহিকতার বিপরীতে, যা একটি সংখ্যার সীমানা দ্বারা সূচিকৃত, ডিকশনারি চাবিগুলি দ্বারা সূচিকৃত, যা যেকোনো অপরিবর্তনীয় টাইপ হতে পারে। এই নকশা পছন্দটি চাবিগুলি অনন্য মানগুলিতে ম্যাপ করা দ্রুত লুকআপ টেবিলের জন্য ডিকশনারিকে আদর্শ করে তোলে।

ঐতিহাসিকভাবে, পাইথন ডিকশনারি একটি হ্যাশ টেবিল ব্যবহার করে বাস্তবায়িত হয়েছে, যা নির্দেশ করে যে লুকআপ, ইনসার্ট, এবং মুছে ফেলার ক্রিয়াকলাপের গড় সময়ের জটিলতা O(1)। পাইথন 3.6 এবং পরবর্তী সংস্করণে, ডিকশনারি আইটেমগুলির ইনসার্টের অর্ডারও বজায় রাখে, হ্যাশ টেবিলের সুবিধাগুলির সাথে অর্ডার করা ডাটা কাঠামোগুলিতে দেখা গেছে ইনসারশন অর্ডারের পূর্বানুমানের সমন্বয় করে।

যদিও ডিকশনারি অত্যন্ত বহুমুখী, কিছু বিশেষজ্ঞ ক্ষেত্রে, `collections.defaultdict` বা `collections.OrderedDict` (পাইথন 3.7 এর আগে) বেশি প্রাধান্য পেতে পারে। `defaultdict` বিশেষভাবে উপযোগী যখন আপনি চান একটি ডিকশনারি অ-অস্তিত্বের কীগুলির জন্য একটি ডিফল্ট মান ফেরত দেয়, নির্দিষ্ট ধরনের শর্তাধীন যুক্তি সহজীকরণ করে। তবে, পাইথনের অবিরত উন্নীতি এবং বিবর্তনের সাথে, বিল্ট-ইন ডিকশনারি ক্লাস প্রায়শই associative arrays হিসেবে যাওয়ার জন্য পছন্দসই পছন্দ হিসাবে থাকে এর দৃঢ়তা এবং বক্স থেকে বের করার সুবিধার জন্য।
