---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:10:44.379220-06:00
description: "Ruby \u09A4\u09C7 \u0995\u09AE\u09BE\u09A8\u09CD\u09A1-\u09B2\u09BE\u0987\
  \u09A8 \u0986\u09B0\u09CD\u0997\u09C1\u09AE\u09C7\u09A8\u09CD\u099F\u09B8 \u09AA\
  \u09A1\u09BC\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2\u09CB \u09B8\u09CD\u0995\
  \u09CD\u09B0\u09BF\u09AA\u09CD\u099F\u09B8 \u099A\u09BE\u09B2\u09BE\u09A8\u09CB\u09B0\
  \ \u09B8\u09AE\u09AF\u09BC \u09B8\u09B0\u09BE\u09B8\u09B0\u09BF \u0987\u09A8\u09AA\
  \u09C1\u099F \u09A8\u09C7\u0993\u09AF\u09BC\u09BE, \u09AF\u09C7\u09AE\u09A8 \u0995\
  \u09A8\u09AB\u09BF\u0997\u09BE\u09B0\u09C7\u09B6\u09A8 \u0985\u09AA\u09B6\u09A8\
  \ \u09AC\u09BE \u09A1\u09BE\u099F\u09BE \u09AA\u09BE\u09B8\u09BF\u0982\u0964 \u09AA\
  \u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09A4\u09BE\
  \u09A6\u09C7\u09B0\u2026"
lastmod: '2024-03-17T18:47:44.606886-06:00'
model: gpt-4-0125-preview
summary: "Ruby \u09A4\u09C7 \u0995\u09AE\u09BE\u09A8\u09CD\u09A1-\u09B2\u09BE\u0987\
  \u09A8 \u0986\u09B0\u09CD\u0997\u09C1\u09AE\u09C7\u09A8\u09CD\u099F\u09B8 \u09AA\
  \u09A1\u09BC\u09BE \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2\u09CB \u09B8\u09CD\u0995\
  \u09CD\u09B0\u09BF\u09AA\u09CD\u099F\u09B8 \u099A\u09BE\u09B2\u09BE\u09A8\u09CB\u09B0\
  \ \u09B8\u09AE\u09AF\u09BC \u09B8\u09B0\u09BE\u09B8\u09B0\u09BF \u0987\u09A8\u09AA\
  \u09C1\u099F \u09A8\u09C7\u0993\u09AF\u09BC\u09BE, \u09AF\u09C7\u09AE\u09A8 \u0995\
  \u09A8\u09AB\u09BF\u0997\u09BE\u09B0\u09C7\u09B6\u09A8 \u0985\u09AA\u09B6\u09A8\
  \ \u09AC\u09BE \u09A1\u09BE\u099F\u09BE \u09AA\u09BE\u09B8\u09BF\u0982\u0964 \u09AA\
  \u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09A4\u09BE\
  \u09A6\u09C7\u09B0\u2026"
title: "\u0995\u09AE\u09BE\u09A8\u09CD\u09A1 \u09B2\u09BE\u0987\u09A8 \u0986\u09B0\
  \u09CD\u0997\u09C1\u09AE\u09C7\u09A8\u09CD\u099F\u0997\u09C1\u09B2\u09BF \u09AA\u09A1\
  \u09BC\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?

Ruby তে কমান্ড-লাইন আর্গুমেন্টস পড়া মানে হলো স্ক্রিপ্টস চালানোর সময় সরাসরি ইনপুট নেওয়া, যেমন কনফিগারেশন অপশন বা ডাটা পাসিং। প্রোগ্রামাররা তাদের স্ক্রিপ্টকে ডাইনামিক এবং অ্যাডাপ্টেবল করতে হার্ডকোডিং মান ছাড়াই এগুলো ব্যবহার করেন।

## কিভাবে:
কমান্ড-লাইন আর্গুমেন্টগুলি ধরতে, রুবি একটি সাধারণ অ্যারে প্রদান করে: `ARGV`। এটি দেওয়া আর্গুমেন্টসগুলি ক্রমে ধারণ করে।

```Ruby
# hello.rb
name = ARGV[0] || "পৃথিবী"
puts "হ্যালো, #{name}!"

# চালানো হয়েছে: ruby hello.rb Alice
# আউটপুট: হ্যালো, Alice!
```

একাধিক আর্গুমেন্ট পরিচালনা করতে:

```Ruby
# greet.rb
name, time_of_day = ARGV
puts "শুভ #{time_of_day || 'দিন'}, #{name || 'সেখানে'}!"

# চালানো হয়েছে: ruby greet.rb Bob Morning
# আউটপুট: শুভ সকাল, Bob!
```

একটি লুপের সাহায্যে অপশন তৈরি করুন:

```Ruby
# options.rb
options = {}
ARGV.each do |arg|
  key, value = arg.split('=')
  options[key.to_sym] = value
end
p options

# চালানো হয়েছে: ruby options.rb name=Tom age=30
# আউটপুট: {:name=>"Tom", :age=>"30"}
```

## গভীরে গিয়ে:
কমান্ড-লাইন আর্গুমেন্টস পড়া প্র্যাকটিস হলো কমান্ড-লাইন ইন্টারফেসগুলির আবির্ভাবের পুরোনো একটি পদ্ধতি। এটি GUI ছাড়া ইউজার ইনপুট ব্যবহার করার কথা বলে—অটোমেশনের জন্য অপরিহার্য বা সার্ভারে স্ক্রিপ্টগুলি চালানোর সময়।

রুবির `ARGV` অনন্য নয়; অনেক ভাষায় এর মতো কিছু আছে। তবে, রুবির বাস্তবায়নে সিম্পলিসিটি এবং স্পষ্ট সিনট্যাক্সের উপর জোর দেওয়া হয়েছে—কোনো ঝামেলা নেই, শুধু একটি অ্যারে।

গভীরে, `ARGV` একটি `Array`-এর একটি উদাহরণ মাত্র, যা কমান্ড কলে স্ক্রিপ্ট নামের পরে প্রদত্ত আর্গুমেন্টগুলি দিয়ে পূর্ব-পূরিত হয়। আপনার কোড চালানোর আগেই রুবি এটি সেটআপ করে, এতে এটি ব্যবহারের জন্য তাত্ক্ষণিকভাবে প্রস্তুত থাকে।

বিকল্প? অবশ্যই। জটিল প্রয়োজনের জন্য, যেমন ফ্ল্যাগ (`--verbose` বা `-v` এর মতো) পার্স করা, রুবির স্ট্যান্ডার্ড লাইব্রেরিতে `OptionParser` ক্লাস আছে। এটি `ARGV` -এর চেয়ে বেশি নিয়ন্ত্রণ করতে পারে, যেমন ডিফল্ট মান, অটোমেটিক টাইপ কনভার্সন, এবং হেল্প মেসেজ জেনারেট করা।

কখনো কখনো, শুধু এটি জানা দরকার যে একটি আর্গুমেন্ট প্রদান করা হয়েছে নাকি না, এর মান বাদ দিয়ে। সে ক্ষেত্রে, `ARGV.include?` কাজে লাগে।

## আরও দেখুন
- `OptionParser` এ একটি ভূমিকা: [https://ruby-doc.org/stdlib-2.7.0/libdoc/optparse/rdoc/OptionParser.html](https://ruby-doc.org/stdlib-2.7.0/libdoc/optparse/rdoc/OptionParser.html)
- রুবিতে কমান্ড-লাইন আর্গুমেন্টস সম্পর্কে আরও: [https://www.rubyguides.com/2018/12/ruby-argv/](https://www.rubyguides.com/2018/12/ruby-argv/)
