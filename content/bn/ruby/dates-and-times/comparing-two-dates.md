---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:46:05.662518-06:00
description: "\u09A6\u09C1\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996\u09C7\u09B0\
  \ \u09A4\u09C1\u09B2\u09A8\u09BE \u09AE\u09BE\u09A8\u09C7 \u09A4\u09BE \u09B8\u09AE\
  \u09BE\u09A8 \u0995\u09BF\u09A8\u09BE \u09AF\u09BE\u099A\u09BE\u0987 \u0995\u09B0\
  \u09BE, \u0985\u09A5\u09AC\u09BE \u0995\u09CB\u09A8\u099F\u09BF \u0985\u09A8\u09CD\
  \u09AF\u099F\u09BF\u09B0 \u0986\u0997\u09C7 \u09AC\u09BE \u09AA\u09B0\u09C7 \u0986\
  \u09B8\u09C7 \u09A4\u09BE \u09AC\u09C7\u09B0 \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\
  \u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u0998\u099F\u09A8\
  \u09BE\u09AC\u09B2\u09C0 \u099F\u09CD\u09B0\u09CD\u09AF\u09BE\u0995 \u0995\u09B0\
  \u09A4\u09C7, \u09B0\u09BF\u099C\u09BE\u09B0\u09CD\u09AD\u09C7\u09B6\u09A8 \u09B8\
  \u09AE\u09CD\u09AA\u09BE\u09A6\u09A8 \u0995\u09B0\u09A4\u09C7,\u2026"
lastmod: '2024-03-17T18:47:44.603595-06:00'
model: gpt-4-0125-preview
summary: "\u09A6\u09C1\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996\u09C7\u09B0 \u09A4\
  \u09C1\u09B2\u09A8\u09BE \u09AE\u09BE\u09A8\u09C7 \u09A4\u09BE \u09B8\u09AE\u09BE\
  \u09A8 \u0995\u09BF\u09A8\u09BE \u09AF\u09BE\u099A\u09BE\u0987 \u0995\u09B0\u09BE\
  , \u0985\u09A5\u09AC\u09BE \u0995\u09CB\u09A8\u099F\u09BF \u0985\u09A8\u09CD\u09AF\
  \u099F\u09BF\u09B0 \u0986\u0997\u09C7 \u09AC\u09BE \u09AA\u09B0\u09C7 \u0986\u09B8\
  \u09C7 \u09A4\u09BE \u09AC\u09C7\u09B0 \u0995\u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u0998\u099F\u09A8\u09BE\
  \u09AC\u09B2\u09C0 \u099F\u09CD\u09B0\u09CD\u09AF\u09BE\u0995 \u0995\u09B0\u09A4\
  \u09C7, \u09B0\u09BF\u099C\u09BE\u09B0\u09CD\u09AD\u09C7\u09B6\u09A8 \u09B8\u09AE\
  \u09CD\u09AA\u09BE\u09A6\u09A8 \u0995\u09B0\u09A4\u09C7, \u099F\u09BE\u0987\u09AE\
  \u09B2\u09BE\u0987\u09A8 \u09B8\u09BE\u099C\u09BE\u09A8\u09CB, \u098F\u09AC\u0982\
  \ \u09B8\u09AE\u09AF\u09BC\u09C7\u09B0 \u0995\u09CD\u09B0\u09AE \u09AC\u09BF\u09B7\
  \u09AF\u09BC\u0995 \u09AF\u09C7\u0995\u09CB\u09A8 \u0995\u09BE\u099C\u09C7\u09B0\
  \ \u099C\u09A8\u09CD\u09AF \u098F\u0987 \u09A4\u09C1\u09B2\u09A8\u09BE \u0995\u09B0\
  \u09C7 \u09A5\u09BE\u0995\u09C7\u09A8\u0964."
title: "\u09A6\u09C1\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996 \u09A4\u09C1\u09B2\
  \u09A8\u09BE \u0995\u09B0\u09BE"
weight: 27
---

## কি এবং কেন?

দুটি তারিখের তুলনা মানে তা সমান কিনা যাচাই করা, অথবা কোনটি অন্যটির আগে বা পরে আসে তা বের করা। প্রোগ্রামাররা ঘটনাবলী ট্র্যাক করতে, রিজার্ভেশন সম্পাদন করতে, টাইমলাইন সাজানো, এবং সময়ের ক্রম বিষয়ক যেকোন কাজের জন্য এই তুলনা করে থাকেন।

## কিভাবে:

রুবি তার তারিখ ক্লাসের মাধ্যমে আমাদের জীবনকে সহজ করে দিয়েছে। চলুন এটিকে ব্যবহার করে দেখি।

```ruby
require 'date'

date1 = Date.new(2023, 3, 14)
date2 = Date.new(2023, 3, 15)

puts date1 == date2   # আউটপুট: false
puts date1 != date2   # আউটপুট: true
puts date1 < date2    # আউটপুট: true
puts date1 > date2    # আউটপুট: false
puts date1 <= Date.today # আউটপুট: আজকের দিনের উপর নির্ভরশীল
puts date1 >= Date.today # আউটপুট: আজকের দিনের উপর নির্ভরশীল
```

## গভীরে ডুব দেওয়া

তারিখের তুলনা নতুন কিছু নয়। এটি পূর্বাভাষক, যেমনকি পূর্ণসংখ্যাগুলি তুলনা করা, তবে এটি আরও জটিল কারণ তারিখের কয়েকটি অংশ রয়েছে—দিন, মাস, বছর। রুবিতে, স্ট্যান্ডার্ড লাইব্রেরী থেকে Date ক্লাস এই ভার বহন করে, মাস, লিপ বছর ইত্যাদি নিয়ে সামলায়।

আপনি মৌলিক তুলনা দেখেছেন: `==`, `!=`, `<`, `>`, `<=`, `>=`. কিন্তু রুবির আরও অনেক কিছু রয়েছে গোপনে।

* `Date.parse` স্ট্রিং তারিখগুলিকে বোঝা এবং রূপান্তর করতে পারে।
* `DateTime` আরও নির্ভুলতা প্রদান করে, সময় এবং সময়ক্ষেত্র সমর্থন সহ।
* 'ActiveSupport' মতো লাইব্রেরি (রেইলস থেকে) আরও বেশি তারিখ-সংক্রান্ত পদ্ধতি যোগ করে।

পিটফলগুলির জন্য সাবধান থাকুন:
* সময়ক্ষেত্র সাবধান না থাকলে আপনাকে বিভ্রান্ত করতে পারে।
* লিপ সেকেন্ডগুলি রুবির স্ট্যান্ডার্ড Date/DateTime ক্লাসগুলিতে গণনা করা হয় না।

Date ক্লাসের বিকল্পগুলি হলঃ

* টাইমস্ট্যাম্পগুলি ব্যবহার করা এবং তাদেরকে সংখ্যা হিসেবে তুলনা করা।
* আরও উন্নত সময় পরিচালনার জন্য 'time' লাইব্রেরি।

তুলনা দ্রুত জটিল হয়ে ওঠে। যদি আপনি সময়সূচী সাজাচ্ছেন এবং তারিখের পরিসরগুলি তুলনা করতে হয়, অথবা পুনরাবৃত্তি ঘটনাগুলি সামলাচ্ছেন? রুবির Date এবং Time এর উপর নির্মিত উচ্চ-স্তরের সংজ্ঞাবলী প্রায়ই প্রয়োজন হয়। ActiveRecord's `between?` পদ্ধতি অথবা পুনরাবৃত্তি ঘটনাগুলির জন্য 'IceCube' মতো গেমস প্রচুর সময় এবং মাথাব্যথা বাঁচাতে পারে।

## আরও দেখুন

- ActiveSupport's extensions: [Active Support Core Extensions](https://edgeguides.rubyonrails.org/active_support_core_extensions.html)
- পুনরাবৃত্তি ঘটনাগুলির জন্য 'IceCube' গেম: [IceCube](https://github.com/seejohnrun/ice_cube)
- রুবিতে সময়ক্ষেত্রের সম্পূর্ণ গাইড: [Timezone guides](https://thoughtbot.com/blog/its-about-time-zones)
