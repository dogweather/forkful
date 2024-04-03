---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:31:34.779428-06:00
description: "TOML \u09B9\u09B2 \u098F\u0995\u099F\u09BF \u0995\u09A8\u09AB\u09BF\u0997\
  \ \u09AB\u09BE\u0987\u09B2 \u09AB\u09B0\u09AE\u09C7\u099F \u09AF\u09BE \u098F\u09B0\
  \ \u09B8\u09CD\u09AA\u09B7\u09CD\u099F \u09B8\u09BF\u09AE\u09CD\u09AF\u09BE\u09A8\
  \u09CD\u099F\u09BF\u0995\u09CD\u09B8\u09C7\u09B0 \u0995\u09BE\u09B0\u09A3\u09C7\
  \ \u09AA\u09A1\u09BC\u09BE \u09B8\u09B9\u099C\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\
  \u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE XML-\u098F\u09B0 \u09AD\u09BE\u09B0\
  \ \u09AC\u09BE YAML-\u098F\u09B0 \u0985\u09A6\u09CD\u09AD\u09C1\u09A4\u09A4\u09BE\
  \ \u099B\u09BE\u09A1\u09BC\u09BE\u0987 \u0985\u09CD\u09AF\u09BE\u09AA \u0995\u09A8\
  \u09AB\u09BF\u0997 \u098F\u09AC\u0982 \u09A1\u09BE\u099F\u09BE\u2026"
lastmod: '2024-03-17T18:47:44.614967-06:00'
model: gpt-4-0125-preview
summary: "TOML \u09B9\u09B2 \u098F\u0995\u099F\u09BF \u0995\u09A8\u09AB\u09BF\u0997\
  \ \u09AB\u09BE\u0987\u09B2 \u09AB\u09B0\u09AE\u09C7\u099F \u09AF\u09BE \u098F\u09B0\
  \ \u09B8\u09CD\u09AA\u09B7\u09CD\u099F \u09B8\u09BF\u09AE\u09CD\u09AF\u09BE\u09A8\
  \u09CD\u099F\u09BF\u0995\u09CD\u09B8\u09C7\u09B0 \u0995\u09BE\u09B0\u09A3\u09C7\
  \ \u09AA\u09A1\u09BC\u09BE \u09B8\u09B9\u099C\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\
  \u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE XML-\u098F\u09B0 \u09AD\u09BE\u09B0\
  \ \u09AC\u09BE YAML-\u098F\u09B0 \u0985\u09A6\u09CD\u09AD\u09C1\u09A4\u09A4\u09BE\
  \ \u099B\u09BE\u09A1\u09BC\u09BE\u0987 \u0985\u09CD\u09AF\u09BE\u09AA \u0995\u09A8\
  \u09AB\u09BF\u0997 \u098F\u09AC\u0982 \u09A1\u09BE\u099F\u09BE \u09B8\u09BF\u09B0\
  \u09BF\u09AF\u09BC\u09BE\u09B2\u09BE\u0987\u099C\u09C7\u09B6\u09A8 \u09AE\u09CD\u09AF\
  \u09BE\u09A8\u09C7\u099C \u0995\u09B0\u09A4\u09C7 TOML \u09AC\u09CD\u09AF\u09AC\u09B9\
  \u09BE\u09B0 \u0995\u09B0\u09C7\u0964."
title: "\u099F\u09AE\u09B2 \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\
  \u09B0\u09BE"
weight: 39
---

## কি এবং কেন?

TOML হল একটি কনফিগ ফাইল ফরমেট যা এর স্পষ্ট সিম্যান্টিক্সের কারণে পড়া সহজ। প্রোগ্রামাররা XML-এর ভার বা YAML-এর অদ্ভুততা ছাড়াই অ্যাপ কনফিগ এবং ডাটা সিরিয়ালাইজেশন ম্যানেজ করতে TOML ব্যবহার করে।

## কিভাবে:

প্রথমে, `toml-rb` গেম ইনস্টল করুন। এটি Ruby-তে TOML পার্সিংয়ের জন্য একটি জনপ্রিয় পছন্দ।

```Ruby
gem install toml-rb
```

এরপর, একটি TOML ফাইল পড়ুন:

```Ruby
require 'toml-rb'

toml_content = File.read('config.toml')
config = TomlRB.parse(toml_content)
puts config['title']
```

নমুনা আউটপুট হতে পারে:

```
My Awesome App
```

TOML ফাইলে লেখা:

```Ruby
require 'toml-rb'

config = {
  'title' => 'My Awesome App',
  'owner' => {
    'name' => 'John Doe',
    'dob' => Date.new(1979, 5, 27)
  }
}

toml_string = TomlRB.dump(config)
File.write('config.toml', toml_string)
```

`config.toml` চেক করুন এবং আপনি দেখবেন আপনার সেটিংস সুন্দর ভাবে সংরক্ষিত হয়েছে।

## গভীর ডাইভ

TOML, যার পূর্ণরূপ Tom's Obvious, Minimal Language, তা তৈরি করা হয়েছিল টম প্রেস্টন-ওয়ার্নার দ্বারা, যিনি GitHub-এর সহ-প্রতিষ্ঠাতা, প্রায় 2013 সালে। এর প্রাথমিক লক্ষ্য ছিল একটি সরল ফরমেট হতে যা ডাটা স্ট্রাকচারে সহজে পার্স করা যায়। যেখানে JSON এপিআই এর জন্য দারুণ, এবং YAML নমনীয়, সেখানে TOML এর নিচে হল এর মানুষ-বান্ধব জোর দেওয়া। YAML যা ইন্ডেন্টেশন সাথে বিবাদিত হতে পারে, তুলনায় TOML একটি ইনি-লাইক স্ট্রাকচারের দিকে লক্ষ্য রাখে যা অনেকে সহজ এবং ত্রুটির প্রবণতা কম মনে করে।

JSON, YAML, বা XML মতো বিকল্পগুলি প্রতিটির নিজস্ব শক্তি আছে, তবে TOML এমন পরিস্থিতিতে সজীবভাবে উঠে আসে যেখানে একটি কনফিগকে মানুষ এবং প্রোগ্রাম দুজনের দ্বারাই সহজে রক্ষণাবেক্ষণ করা উচিত। এটি শুধু সহজ নয়, বরং কঠোর এবং পঠনযোগ্য ফরম্যাটিং বজায় রাখে। 

প্রযুক্তিগত দিক থেকে, Ruby-তে TOML কনটেন্ট পার্স করার জন্য, আমরা `toml-rb` মত গেম ব্যবহার করি। এই গেমটি Ruby-এর ডায়নামিক প্রকৃতির সুবিধা নিয়ে, TOML ডেটাকে নেটিভ Ruby হ্যাশেস, অ্যারে, এবং অন্যান্য মৌলিক ডাটা কাঠামোগুলিতে রূপান্তরিত করে। এই রূপান্তরের অর্থ হল যে ডেভেলপাররা পরিচিত Ruby সেমান্টিক্স এবং পদ্ধতিগুলি ব্যবহার করে TOML ডেটার সাথে কাজ করতে পারেন।

## আরো দেখুন

- TOML প্রকল্প এবং স্পেসিফিকেশন: https://toml.io/en/
- `toml-rb` গেম: https://github.com/emancu/toml-rb
- TOML, YAML, এবং JSON তুলনা: https://blog.theodo.com/2021/08/compare-yml-toml-json/
