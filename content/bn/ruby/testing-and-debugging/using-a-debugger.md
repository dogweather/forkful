---
title:                "ডিবাগার ব্যবহার করা"
date:                  2024-03-17T18:22:36.098744-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?

রুবি প্রোগ্রামিং ভাষায় ডিবাগার ব্যবহার করা প্রোগ্রামারদের তাদের কোড থামানো, ভেরিয়েবল পরীক্ষা করা, এবং লাইন বাই লাইন তাদের কোডের মধ্য দিয়ে পা চালানোর এক অসাধারণ ক্ষমতা দেয়। বাগ দূরীকরণ, কোড ফ্লো বুঝতে, এবং জাদু হওয়ার সময় বা না হওয়ার সময় তাদের লেখা কোড ঠিক কি করে দেখার জন্য মানুষ এটা করে থাকে। 

## কিভাবে:

রুবির সাথে একটি বিল্ট-ইন ডিবাগার আসে যার নাম `byebug`। প্রথমে, আপনার Gemfile-এ `byebug` অন্তর্ভুক্ত করুন এবং `bundle install` চালান। তারপর, আপনার প্রোগ্রামটি যেখানে বিরতি নিতে চান সেখানে `byebug` স্থাপন করুন।

```Ruby
require 'byebug'

def calculate_magic(number)
  byebug
  magic_number = number * 7
  return magic_number
end

puts calculate_magic(6)
```

এই স্ক্রিপ্টটি চালানো হলে, `byebug`-এ কার্যক্রম স্থগিত হবে, এবং আপনি একটি ইন্টার‌্যাক্টিভ সেশনে প্রবেশ করবেন যেখানে আপনি যেমন কমান্ড টাইপ করতে পারেন:

```
step
next
continue
var local
```

নমুনা আউটপুট এইরূপ একটি প্রম্প্ট দেবে:

```
[2, 11] in example.rb
    2: 
    3: def calculate_magic(number)
    4:   byebug
=>  5:   magic_number = number * 7
    6:   return magic_number
    7: end
    8: 
    9: puts calculate_magic(6)
(byebug) 
```

## গভীরে ডুব:

`byebug` এর আগে, রুবি ডেভেলপাররা `debugger` এবং `pry` ব্যবহার করতো। `pry` শুধু একটি ডিবাগার নয়, এটি একটি শক্তিশালী REPL যা `binding.pry` ব্রেকপয়েন্ট দিয়ে ডিবাগিংয়ের জন্যও ব্যবহার করা যায়।

রুবির `byebug` এর বিকল্পগুলো অন্তর্ভুক্ত করে `pry-byebug`, যা `pry` এর সাথে `byebug` ফাংশনালিটি সমন্বিত করে, এবং `ruby-debug`, যা একটি পুরানো জেম যা সক্রিয়ভাবে রক্ষণাবেক্ষণ করা হয় না।

আপনি যখন `byebug` আহ্বান করেন, ডিবাগারটি আপনার কোড কার্যকরণ স্থগিত করে এবং রানটাইমে একটি ঝলক দেখার সুযোগ দেয়। আপনি ভেরিয়েবল দেখতে এবং পরিবর্তন করতে, কোডের ভিন্ন ভিন্ন স্থানে লাফিয়ে যেতে, এবং এমনকি কিছু রুবি কোড লাইন বাই লাইন চালাতে পারেন। এটি আপনার রুবি কোডের জন্য সময় ভ্রমণের মতো ক্ষমতা হতে পারে।

## আরও দেখুন:

- Byebug GitHub Repository: [https://github.com/deivid-rodriguez/byebug](https://github.com/deivid-rodriguez/byebug)
- Pry ডকুমেন্টেশন: [https://github.com/pry/pry](https://github.com/pry/pry)
- রেলস অ্যাপস ডিবাগ করার গাইড: [https://guides.rubyonrails.org/debugging_rails_applications.html](https://guides.rubyonrails.org/debugging_rails_applications.html)
