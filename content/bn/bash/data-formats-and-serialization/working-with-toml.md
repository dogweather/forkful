---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:29:57.210118-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09AA\u09CD\u09B0\u09A5\u09AE\
  \u09C7, TOML \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 Bash \u098F \u0996\u09C7\u09B2\
  \u09BE\u09B0 \u099C\u09A8\u09CD\u09AF `toml-cli` \u0987\u09A8\u09B8\u09CD\u099F\u09B2\
  \ \u0995\u09B0\u09C1\u09A8\u0964 TOML \u09AB\u09BE\u0987\u09B2\u0997\u09C1\u09B2\
  \u09BF \u09AA\u09A1\u09BC\u09A4\u09C7 \u09AC\u09BE \u09B8\u09AE\u09CD\u09AA\u09BE\
  \u09A6\u09A8\u09BE \u0995\u09B0\u09A4\u09C7 \u09B9\u09BE\u09A4\u09C7\u09B0 \u0995\
  \u09BE\u099B\u09C7\u0964."
lastmod: '2024-03-17T18:47:44.251530-06:00'
model: gpt-4-0125-preview
summary: "\u09AA\u09CD\u09B0\u09A5\u09AE\u09C7, TOML \u098F\u09B0 \u09B8\u09BE\u09A5\
  \u09C7 Bash \u098F \u0996\u09C7\u09B2\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF `toml-cli`\
  \ \u0987\u09A8\u09B8\u09CD\u099F\u09B2 \u0995\u09B0\u09C1\u09A8\u0964 TOML \u09AB\
  \u09BE\u0987\u09B2\u0997\u09C1\u09B2\u09BF \u09AA\u09A1\u09BC\u09A4\u09C7 \u09AC\
  \u09BE \u09B8\u09AE\u09CD\u09AA\u09BE\u09A6\u09A8\u09BE \u0995\u09B0\u09A4\u09C7\
  \ \u09B9\u09BE\u09A4\u09C7\u09B0 \u0995\u09BE\u099B\u09C7\u0964."
title: "\u099F\u09AE\u09B2 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\
  \u09B0\u09BE"
weight: 39
---

## কিভাবে:
প্রথমে, TOML এর সাথে Bash এ খেলার জন্য `toml-cli` ইনস্টল করুন। TOML ফাইলগুলি পড়তে বা সম্পাদনা করতে হাতের কাছে।

```Bash
# toml-cli ইনস্টল করুন, আমাদের TOML টাস্কের জন্য ছোট্ট সহায়ক
pip install toml-cli

# কল্পনা করুন আপনার কাছে একটি TOML ফাইল আছে, 'config.toml'
echo -e 'title = "TOML ডেমো"\n\n[owner]\nname = "Tom"\ndob = 1979-05-27T07:32:00Z' > config.toml

# একটি মান পড়ুন
toml get config.toml owner.name
# আউটপুট: Tom

# একটি মান সেট করুন
toml set config.toml 'owner.dob' '2000-01-01T00:00:00Z'
# প্রো টিপ: ডটস বা ফাংকি চরিত্রযুক্ত কীগুলির জন্য উদ্ধৃতি ব্যবহার করুন!
```

## গভীর ডাইভ
JSON-এর মানুষের জন্য বাধাগুলির অপছন্দ থেকে, TOML ২০১৩ সালে আত্মপ্রকাশ করে। গিটহাবের সহ-প্রতিষ্ঠাতা টম প্রেস্টন-ওয়ার্নার কিছু সুপার পাঠযোগ্য চেয়েছিলেন। YAML এবং INI ছিল বিকল্প কিন্তু TOML উভয়ের সেরা পার্টগুলির মত।

শেবাং, আপনার কাছে নেস্টেড ডাটা এবং অ্যারে আছে, YAML-এর ফুট গান্স এবং JSON-এর কার্লি ব্রেসের বিনা প্রয়োজনে। Rust-এর Cargo-তে কনফিগের জন্য TOML এখন একটি যাওয়ার জায়গা, যা ডেভলপার বিশ্বে এর উন্নতি প্রদর্শন করে। একটি স্পেক দ্বারা চালিত হয়, জিনিসগুলিকে টাইট এবং ভালভাবে-ডিফাইন্ড রাখে। প্রায় যেকোনো ভাষায় পার্সার পাওয়া যায়, তাই এটি ব্যাপকভাবে নেওয়া যেতে পারে।

## আরও দেখুন
- অফিসিয়াল TOML GitHub রেপো: https://github.com/toml-lang/toml
- PyPI তে toml-cli: https://pypi.org/project/toml-cli/
- ডাটা-সিরিয়ালাইজেশন ফরম্যাটগুলির তুলনা: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
