---
title:                "স্ট্রিং থেকে উদ্ধৃতি মুছে ফেলা"
date:                  2024-03-17T18:13:51.875959-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?
একটি স্ট্রিং থেকে উদ্ধৃতি চিহ্ন অপসারণ মানে টেক্সট মানকে ঘিরে থাকা সেই ডাবল বা সিঙ্গেল উদ্ধৃতি চিহ্নগুলো তুলে নেওয়া। প্রোগ্রামাররা প্রায়ই ডেটা প্রক্রিয়াকরণে ধারাবাহিকতা নিশ্চিত করতে বা এমন সিস্টেমের জন্য ডেটা প্রস্তুত করতে যা এই অতিরিক্ত অক্ষরগুলো দ্বারা বিভ্রান্ত হতে পারে, ব্যবহারকারীর ইনপুট পরিষ্কার করার জন্য এটি করে।

## কিভাবে:
রুবির সেই বিরক্তিকর উদ্ধরণ চিহ্নগুলি বাদ দেওয়ার জন্য কিছু দারুন কৌশল আছে। আপনি `gsub` অথবা `delete` মেথডস ব্যবহার করে কাজটি সমাধান করতে পারেন। এখানে কিছু কোড দেওয়া হলো যা নিয়ে চিন্তা করা যায়:

```ruby
# ডাবল এবং সিঙ্গেল উদ্ধৃতি অপসারণের জন্য gsub ব্যবহার করা
quoted_string = "\"Say 'hello' to my little friend!\""
unquoted_string = quoted_string.gsub(/'|"/, '')
puts unquoted_string 
# আউটপুট: Say hello to my little friend!

# যদি আপনি জানেন আপনি শুধুমাত্র এক ধরনের উদ্ধৃতির সাথে যুক্ত হতে যাচ্ছেন
single_quoted_string = "'Stay a while and listen!'"
clean_string = single_quoted_string.delete("'")
puts clean_string 
# আউটপুট: Stay a while and listen!
```

## গভীর ডাইভ
উদ্ধৃতি চিহ্নের ইতিহাস প্রোগ্রামিংয়ের প্রথম সময় পর্যন্ত ফিরে যায়, যেখানে তারা প্রায়শই স্ট্রিং ডিলিমিটারগুলির ভূমিকা পালন করে। আজকের দিনেও, তারা যেমন তখন ছিল, আপনি নিজেকে এই উদ্ধরণ অক্ষরগুলি অপসারণের প্রয়োজনে খুঁজে পেতে পারেন যখন তারা প্রয়োজন হয় না বা যখন তারা ডেটা সংরক্ষণ এবং ম্যানিপুলেশনের সাথে বাধাগ্রস্থ হতে পারে।

আমরা `gsub` এবং `delete` সম্পর্কে আলোচনা করেছি কিন্তু অন্যান্য মেথডগুলিও আছে, যেমন `tr` অথবা `tr_s`, যা আপনাকে একটু বেশি নিয়ন্ত্রণ দেয় অথবা কিছু ভিন্ন ব্যবহারের ক্ষেত্রে সাহায্য করতে পারে:

```ruby
# tr ও উদ্ধৃতি অপসারণে সহায়ক
double_quoted_string = "\"Do or do not, there is no try.\""
clean_string = double_quoted_string.tr('\"', '')
puts clean_string 
# আউটপুট: Do or do not, there is no try.
```

মনে রাখবেন, প্রতিটি মেথডের তার ব্যবহারের ক্ষেত্র আছে। `gsub` আরও শক্তিশালী যখন আপনি জটিল প্যাটার্ন বা একাধিক প্রতিস্থাপনের সাথে মোকাবিলা করছেন। `delete` এবং `tr` সোজা, সরল অক্ষর অপসারণের জন্য সুন্দর কাজ করে।

## আরো দেখুন
অতিরিক্ত পাঠ এবং বৃহত্তর কোডবেসগুলিতে এই মেথডগুলি কাজে লাগানো দেখতে, দেখুন:
- রুবি ডকুমেন্টেশনে [String#gsub](https://ruby-doc.org/core-3.1.2/String.html#method-i-gsub), [String#delete](https://ruby-doc.org/core-3.1.2/String.html#method-i-delete), এবং [String#tr](https://ruby-doc.org/core-3.1.2/String.html#method-i-tr)।
- Ruby Monstas এর একটি দারুন [String exercise set](http://ruby-for-beginners.rubymonstas.org/built_in_classes/strings.html) রয়েছে, যা উদ্ধৃতিগুলি নিয়ে কাজ করা অন্তর্ভুক্ত।
- Stack Overflow আলোচনাগুলি [string manipulation](https://stackoverflow.com/search?q=ruby+remove+quotes+from+string) বাস্তব বিশ্বের সমস্যা এবং সমাধান সরবরাহ করে, সহকর্মী Rubyists থেকে।
