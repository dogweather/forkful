---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:23:48.990321-06:00
description: "REPL, \u0985\u09B0\u09CD\u09A5\u09BE\u09CE \u09AA\u09A1\u09BC\u09BE\
  -\u09AE\u09C2\u09B2\u09CD\u09AF\u09BE\u09AF\u09BC\u09A8-\u09AE\u09C1\u09A6\u09CD\
  \u09B0\u09A3 \u09B2\u09C1\u09AA, \u098F\u0995 \u09A7\u09B0\u09A8\u09C7\u09B0 \u09AA\
  \u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BF\u0982 \u09AA\u09B0\u09BF\u09AC\
  \u09C7\u09B6 \u09AF\u09BE \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u0995\u09BE\
  \u09B0\u09C0\u09B0 \u098F\u0995\u0995 \u0987\u09A8\u09AA\u09C1\u099F \u0997\u09CD\
  \u09B0\u09B9\u09A3 \u0995\u09B0\u09C7, \u09A4\u09BE \u09A8\u09BF\u09B0\u09CD\u09AC\
  \u09BE\u09B9 \u0995\u09B0\u09C7, \u098F\u09AC\u0982 \u09AB\u09B2\u09BE\u09AB\u09B2\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u0995\u09BE\u09B0\u09C0\u09B0 \u0995\
  \u09BE\u099B\u09C7\u2026"
lastmod: '2024-03-17T18:47:43.574454-06:00'
model: gpt-4-0125-preview
summary: "REPL, \u0985\u09B0\u09CD\u09A5\u09BE\u09CE \u09AA\u09A1\u09BC\u09BE-\u09AE\
  \u09C2\u09B2\u09CD\u09AF\u09BE\u09AF\u09BC\u09A8-\u09AE\u09C1\u09A6\u09CD\u09B0\u09A3\
  \ \u09B2\u09C1\u09AA, \u098F\u0995 \u09A7\u09B0\u09A8\u09C7\u09B0 \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BF\u0982 \u09AA\u09B0\u09BF\u09AC\u09C7\u09B6\
  \ \u09AF\u09BE \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u0995\u09BE\u09B0\u09C0\
  \u09B0 \u098F\u0995\u0995 \u0987\u09A8\u09AA\u09C1\u099F \u0997\u09CD\u09B0\u09B9\
  \u09A3 \u0995\u09B0\u09C7, \u09A4\u09BE \u09A8\u09BF\u09B0\u09CD\u09AC\u09BE\u09B9\
  \ \u0995\u09B0\u09C7, \u098F\u09AC\u0982 \u09AB\u09B2\u09BE\u09AB\u09B2 \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09BE\u09B0\u0995\u09BE\u09B0\u09C0\u09B0 \u0995\u09BE\u099B\u09C7\
  \ \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7\u0964 \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09A6\u09CD\u09B0\u09C1\
  \u09A4 \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE, \u09B6\u09BF\u0995\u09CD\u09B7\
  \u09BE, \u09A1\u09BF\u09AC\u09BE\u0997\u09BF\u0982 \u0985\u09A5\u09AC\u09BE \u099A\
  \u09B2\u09BE\u099A\u09B2\u09C7\u09B0 \u09B8\u09AE\u09AF\u09BC \u09B9\u09BF\u09B8\
  \u09BE\u09AC \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u099F\u09BF\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7\u0964."
title: "\u0987\u09A8\u09CD\u099F\u09BE\u09B0\u09AF\u09BC\u09BE\u0995\u09CD\u099F\u09BF\
  \u09AD \u09B6\u09C7\u09B2 (REPL) \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\
  \u09B0\u09BE"
weight: 34
---

## কিভাবে:
কমান্ড লাইনে `python` টাইপ করে পাইথনের REPL এ সরাসরি প্রবেশ করুন। এখানে থেকে, সহজ অপারেশন বা বহু-লাইনের কোড পরীক্ষা করুন:

```Python
>>> 1 + 1
2
>>> জন্য i মধ্যে পরিসীমা(3):
...     মুদ্রণ(i)
... 
0
1
2
```

ফাংশন এবং তাৎক্ষণিক প্রতিক্রিয়া নিয়ে পরীক্ষা করুন:

```Python
>>> def greet(name):
...     return "Hello, " + name + "!"
... 
>>> greet("Alice")
'Hello, Alice!'
```

লাইব্রেরি নিয়ে খেলুন এবং বাস্তব সময়ে তাদের বৈশিষ্ট্যগুলি অন্বেষণ করুন:

```Python
>>> import math
>>> math.sqrt(16)
4.0
```

দ্রুত `exit()` বা `Ctrl+D` (কখনো কখনো উইন্ডোজে `Ctrl+Z`) দ্বারা প্রস্থান করুন।

## গভীর ডুব
REPL এর ধারণা শুধুমাত্র পাইথনে সীমাবদ্ধ নয়; এটি Lisp যত পুরানো। অনেক ভাষায় এই সাথে, অবিলম্বে, ইন্টার‌্যাক্টিভ পরিবেশ প্রদান করা হয়, যা কোডিংয়ে হাতেকলমে অভিজ্ঞতা দান করে। পাইথনের নেটিভ শেলের পরিবর্তে IPython এবং Jupyter নোটবুক রয়েছে, যা উন্নত ইন্টার‌্যাক্টিভিটি, আরও বৈশিষ্ট্য, এবং অন্যান্য টুলের সাথে উন্নত সমন্বয় প্রদান করে। পাইথনের মানক REPL সরল হলেও, এটি পাইথনের পূর্ণ শক্তি এমবেড করে, জটিল অবজেক্ট এবং মাল্টি-থ্রেডেড প্রোগ্রাম সামলানো, যদিও এটি উন্নত টুলে থাকা অটো-কমপ্লিশন এবং সিনট্যাক্স হাইলাইটিং মতো বৈশিষ্ট্যগুলির অভাব রয়েছে।

## আরও দেখুন
- [ইন্টারপ্রিটার সম্পর্কে পাইথনের আনুষ্ঠানিক ডকুমেন্টেশন](https://docs.python.org/3/tutorial/interpreter.html)
- [IPython: একটি উন্নত পাইথন শেল](https://ipython.org/)
- [জুপিটার প্রজেক্ট](https://jupyter.org/)
