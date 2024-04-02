---
changelog:
- 2024-01-28, dogweather, reviewed
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:50:51.311612-06:00
description: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0987\u09A8\u09CD\u099F\u09BE\
  \u09B0\u09AA\u09CB\u09B2\u09C7\u09B6\u09A8 \u09B9\u09B2 \u09B8\u09CD\u099F\u09CD\
  \u09B0\u09BF\u0982 \u09B2\u09BF\u099F\u09BE\u09B0\u09C7\u09B2\u09C7\u09B0 \u09AE\
  \u09A7\u09CD\u09AF\u09C7 \u098F\u0995\u09CD\u09B8\u09AA\u09CD\u09B0\u09C7\u09B6\u09A8\
  \ \u098F\u09AE\u09CD\u09AC\u09C7\u09A1 \u0995\u09B0\u09BE\u09B0 \u09AA\u09A6\u09CD\
  \u09A7\u09A4\u09BF\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\
  \u09B0\u09B0\u09BE \u098F\u099F\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\
  \ \u0995\u09B0\u09C7\u09A8 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0997\u09C1\
  \u09B2\u09BF\u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7 \u09A1\u09BE\u09AF\u09BC\u09A8\
  \u09BE\u09AE\u09BF\u0995\u09AD\u09BE\u09AC\u09C7 \u09AE\u09BE\u09A8 \u09AA\u09CD\
  \u09B0\u09AC\u09C7\u09B6\u2026"
lastmod: '2024-03-17T18:47:43.556748-06:00'
model: gpt-4-0125-preview
summary: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0987\u09A8\u09CD\u099F\u09BE\
  \u09B0\u09AA\u09CB\u09B2\u09C7\u09B6\u09A8 \u09B9\u09B2 \u09B8\u09CD\u099F\u09CD\
  \u09B0\u09BF\u0982 \u09B2\u09BF\u099F\u09BE\u09B0\u09C7\u09B2\u09C7\u09B0 \u09AE\
  \u09A7\u09CD\u09AF\u09C7 \u098F\u0995\u09CD\u09B8\u09AA\u09CD\u09B0\u09C7\u09B6\u09A8\
  \ \u098F\u09AE\u09CD\u09AC\u09C7\u09A1 \u0995\u09B0\u09BE\u09B0 \u09AA\u09A6\u09CD\
  \u09A7\u09A4\u09BF\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\
  \u09B0\u09B0\u09BE \u098F\u099F\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\
  \ \u0995\u09B0\u09C7\u09A8 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u0997\u09C1\
  \u09B2\u09BF\u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7 \u09A1\u09BE\u09AF\u09BC\u09A8\
  \u09BE\u09AE\u09BF\u0995\u09AD\u09BE\u09AC\u09C7 \u09AE\u09BE\u09A8 \u09AA\u09CD\
  \u09B0\u09AC\u09C7\u09B6\u2026"
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0987\u09A8\u09CD\u099F\u09BE\u09B0\
  \u09AA\u09CB\u09B2\u09C7\u099F \u0995\u09B0\u09BE"
weight: 8
---

## কি এবং কেন?
স্ট্রিং ইন্টারপোলেশন হল স্ট্রিং লিটারেলের মধ্যে এক্সপ্রেশন এম্বেড করার পদ্ধতি। প্রোগ্রামাররা এটি ব্যবহার করেন স্ট্রিংগুলির মধ্যে ডায়নামিকভাবে মান প্রবেশ করানোর জন্য, যা কোডকে পারম্পরিক স্ট্রিং কনক্যাটিনেশনের চেয়ে আরও পাঠযোগ্য এবং পরিষ্কার করে তোলে।

## কিভাবে:
পাইথন 3.6 এবং তার উপরে, আপনি f-স্ট্রিংস ব্যবহার করে স্ট্রিং ইন্টারপোলেট করতে পারেন। এখানে কিভাবে:

```Python
name = 'Alice'
age = 30
greeting = f"হ্যালো, {name}. তুমি {age} বছর বয়সী।"

print(greeting)
```

আউটপুট:
```
হ্যালো, Alice. তুমি 30 বছর বয়সী।
```

আপনি কার্লি ব্র্যাকেটের মধ্যে এক্সপ্রেশনও ব্যবহার করতে পারেন:

```Python
a = 5
b = 10
info = f"পাঁচ প্লাস দশ হল {a + b}, না হয় {2 * (a + b)}."

print(info)
```

আউটপুট:
```
পাঁচ প্লাস দশ হল 15, না হয় 30।
```

## গভীর ডুব
পাইথন 3.6 এর পূর্বে, `.format()` স্ট্রিং ইন্টারপোলেট করার উপায় ছিল:

```Python
name = 'Bob'
age = 25
greeting = "হ্যালো, {}. তুমি {} বছর বয়সী।".format(name, age)

print(greeting)
```

পুরানো স্কুলের পাইথন (ভার্সন < 2.6) ইন্টারপোলেশনের জন্য `%` অপারেটর ব্যবহার করত, যা কম স্বজ্ঞাত এবং একাধিক ভেরিয়েবল সহ জঘন্য হতে পারে:

```Python
name = 'Carol'
age = 35
greeting = "হ্যালো, %s. তুমি %d বছর বয়সী।" % (name, age)

print(greeting)
```

পরিষ্কার সিনট্যাক্সের পাশাপাশি, f-স্ট্রিংগুলি দ্রুতগতির কারণ তারা রানটাইমে মূল্যায়ন করা হয় এবং তারপরে সরাসরি একটি দক্ষ স্ট্রিং ফরম্যাট অপারেশনে পরিণত করা হয়। `.format()` এবং `%` অপারেটরটি আরও অনেক ধাপ ও ধীরে ধীরে কাজ করে।

## দেখুন এছাড়াও
- [PEP 498 – লিটারাল স্ট্রিং ইন্টারপোলেশন](https://www.python.org/dev/peps/pep-0498/) f-স্ট্রিংগুলি সম্পর্কিত অফিসিয়াল ডকুমেন্টেশন জন্য।
- [পাইথন f-স্ট্রিংগুলি](https://realpython.com/python-f-strings/) f-স্ট্রিংগুলি ব্যবহার করে একটি টিউটোরিয়ালের জন্য রিয়েল পাইথন দ্বারা।
- [দ্য .format() মেথড](https://docs.python.org/3/library/stdtypes.html#str.format) পুরানো `.format()` স্ট্রিং ফরম্যাটিং পদ্ধতি বুঝতে পাইথন ডকুমেন্টেশনে।
