---
title:                "টমল নিয়ে কাজ করা"
date:                  2024-03-17T18:30:30.023774-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?
TOML হল একটি কনফিগ ফাইল ফরম্যাট, যা মানবের পড়া এবং লেখা জন্য সহজ, এবং মেশিনের দ্বারা পার্স এবং জেনারেট করা সহজ। প্রোগ্রামাররা TOML নিয়ে কাজ করেন যেখানে পঠনীয়তা গুরুত্বপূর্ণ এমন প্রকল্পের জন্য স্পষ্ট, স্তরবিন্যাসযুক্ত কনফিগ ফাইলগুলির জন্য।

## কিভাবে:
Fish-এ TOML পড়তে এবং ম্যানিপুলেট করতে, আপনি `yj` এর মতো একটি টুল ব্যবহার করতে পারেন, যা TOML কে JSON এ রূপান্তর করতে পারে। এখানে কিভাবে:

```fish
# Fisher এর মাধ্যমে yj ইন্সটল করুন
fisher install jorgebucaran/yj

# TOML কে JSON এ রূপান্তর করুন
echo 'title = "TOML Example"' | yj -tj

# নমুনা আউটপুট
{"title":"TOML Example"}
```

TOML লিখতে, আপনাকে প্রক্রিয়াটি উল্টাতে হবে:

```fish
# JSON কে TOML এ রূপান্তর করুন
echo '{"title":"JSON Example"}' | yj -jt

# নমুনা আউটপুট
title = "JSON Example"
```

ভারী কাজের জন্য, `toml-cli` এর মতো একটি বিশেষায়িত TOML CLI টুল বিবেচনা করুন।

```fish
# toml-cli ইন্সটল করুন
pip install toml-cli

# TOML ফাইলে একটি মান সেট করুন
toml set pyproject.toml tool.poetry.version "1.1.4"

# TOML ফাইল থেকে একটি মান পান
set version (toml get pyproject.toml tool.poetry.version)
echo $version
```

## গভীর দর্শন
TOML (Tom's Obvious, Minimal Language), ২০১৩ সালে Tom Preston-Werner দ্বারা প্রবর্তিত, INI এর মতো কিন্তু একটি নির্দিষ্ট স্পেক এবং ডেটা হায়ারার্কির সাথে। JSON এবং YAML প্রধান বিকল্প, কিন্তু তাদের নিজ নিজ সীমাবদ্ধতা আছে: JSON মানুষের জন্য ততটা বন্ধুত্বপূর্ণ নয়, অপরদিকে YAML আরও জটিল। TOML-এর ডিজাইন এমন পরিস্থিতিতে উজ্জ্বল হয় যেখানে কনফিগ ফাইলগুলি প্রায়শই হাতে বজায় রাখা হয়, সাদাসিধা এবং প্রকাশক্ষমতার মধ্যে ভারসাম্য রেখে। বাস্তবায়নের ক্ষেত্রে, প্রায় সকল প্রোগ্রামিং ভাষায় TOML পার্সার উপলব্ধ, এমনকি Fish-এর জন্য TomlBombadil সহ, যা আপনার স্ক্রিপ্টে সরাসরি যুক্ত করা যাবে।

## আরও দেখুন
- TOML অফিশিয়াল স্পেসিফিকেশন: https://toml.io
- `yj`, TOML, JSON, YAML, এবং XML এর মধ্যে রূপান্তরের একটি টুল: https://github.com/jorgebucaran/yj
- `toml-cli`, TOML-এর জন্য একটি কমান্ড-লাইন ইউটিলিটি: https://github.com/sdispater/toml-cli
