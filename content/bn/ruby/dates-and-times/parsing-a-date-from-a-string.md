---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:06:21.185956-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Ruby \u09A4\u09C7, \u09B8\u09CD\
  \u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1 \u09B2\u09BE\u0987\
  \u09AC\u09CD\u09B0\u09C7\u09B0\u09BF `Date` \u098F\u09AC\u0982 `DateTime` \u0995\
  \u09CD\u09B2\u09BE\u09B8 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\
  \u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u09A4\
  \u09BE\u09B0\u09BF\u0996 \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE\u09B0\
  \ \u099C\u09A8\u09CD\u09AF \u09B8\u09B0\u09BE\u09B8\u09B0\u09BF \u09AA\u09A6\u09CD\
  \u09A7\u09A4\u09BF \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7\u0964\
  \ \u098F\u0996\u09BE\u09A8\u09C7 \u09B0\u09AF\u09BC\u09C7\u099B\u09C7\u2026"
lastmod: '2024-03-17T18:47:44.600541-06:00'
model: gpt-4-0125-preview
summary: "Ruby \u09A4\u09C7, \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\
  \u09BE\u09B0\u09CD\u09A1 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\
  \ `Date` \u098F\u09AC\u0982 `DateTime` \u0995\u09CD\u09B2\u09BE\u09B8 \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\
  \u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u09A4\u09BE\u09B0\u09BF\u0996 \u09AA\u09BE\
  \u09B0\u09CD\u09B8 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u09B8\u09B0\
  \u09BE\u09B8\u09B0\u09BF \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF \u09AA\u09CD\u09B0\
  \u09A6\u09BE\u09A8 \u0995\u09B0\u09C7\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u09B0\
  \u09AF\u09BC\u09C7\u099B\u09C7 \u0995\u09BF\u09AD\u09BE\u09AC\u09C7 Ruby'\u09B0\
  \ \u09A8\u09BF\u09B0\u09CD\u09AE\u09BF\u09A4 \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF\
  \ \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u098F\u099F\u09BF\
  \ \u0995\u09B0\u09BE \u09AF\u09BE\u09AF\u09BC."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u09A4\
  \u09BE\u09B0\u09BF\u0996 \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE"
weight: 30
---

## কিভাবে:
Ruby তে, স্ট্যান্ডার্ড লাইব্রেরি `Date` এবং `DateTime` ক্লাস ব্যবহার করে স্ট্রিং থেকে তারিখ পার্স করার জন্য সরাসরি পদ্ধতি প্রদান করে। এখানে রয়েছে কিভাবে Ruby'র নির্মিত পদ্ধতি ব্যবহার করে এটি করা যায়:

```ruby
require 'date'

# একটি স্ট্রিং থেকে তারিখ পার্স করা
date_string = "2023-04-01"
parsed_date = Date.parse(date_string)
puts parsed_date
# => 2023-04-01

# আরও বিশদ সময় প্রকাশের জন্য DateTime
datetime_string = "2023-04-01T15:30:45+00:00"
parsed_datetime = DateTime.parse(datetime_string)
puts parsed_datetime
# => 2023-04-01T15:30:45+00:00
```

আরও নিয়ন্ত্রণের জন্য অথবা `parse` সরাসরি বুঝতে না পারা ফর্ম্যাটের জন্য, আপনি `strptime` (স্ট্রিং পার্স টাইম) ব্যবহার করতে পারেন, ফর্ম্যাটটি সরাসরি নির্দিষ্ট করে:

```ruby
# কাস্টম ফর্ম্যাটের জন্য strptime ব্যবহার করা
custom_date_string = "01-04-2023"
parsed_date_custom = Date.strptime(custom_date_string, '%d-%m-%Y')
puts parsed_date_custom
# => 2023-04-01
```

### তৃতীয়-পক্ষের লাইব্রেরি ব্যবহার করুন:
Ruby'র নির্মিত ক্ষমতা শক্তিশালী হলেও, কখনও কখনও আপনি অতিরিক্ত বৈশিষ্ট্য বা সহজ সিনট্যাক্সের জন্য তৃতীয়-পক্ষের লাইব্রেরিগুলি পছন্দ করতে পারেন। প্রাকৃতিক ভাষা পার্সিং এর জন্য জনপ্রিয় পছন্দ হল 'Chronic' জেম:

1. প্রথমে, আপনার Gemfile এ Chronic যুক্ত করুন এবং `bundle install` চালান:
```ruby
gem 'chronic'
```

2. তারপর, এটি এইভাবে ব্যবহার করুন:
```ruby
require 'chronic'

parsed_chronic = Chronic.parse('next Tuesday')
puts parsed_chronic
# আউটপুট বর্তমান তারিখের উপর নির্ভরশীল; 2023-04-01 এ পার্সিং ধরে নেয়া হলে
# => 2023-04-04 12:00:00 +0000
```

`Chronic` অ্যাপ্লিকেশনের জন্য একটি শক্তিশালী টুল হতে পারে যা নমনীয় তারিখ এন্ট্রির প্রয়োজন, কারণ এটি প্রাকৃতিক ভাষার একটি বিস্তৃত পরিসীমা তারিখ ফর্ম্যাট বুঝতে পারে।
