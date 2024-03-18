---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:26:17.053644-06:00
description: "\u098F\u09B8\u09CB\u09B8\u09BF\u09AF\u09BC\u09C7\u099F\u09BF\u09AD \u0985\
  \u09CD\u09AF\u09BE\u09B0\u09C7\u0997\u09C1\u09B2\u09BF, \u09AF\u09BE \u09B0\u09C1\
  \u09AC\u09BF \u09AD\u09BE\u09B7\u09BE\u09AF\u09BC \u0985\u09A7\u09BF\u0995\u09BE\
  \u0982\u09B6 \u09B8\u09AE\u09AF\u09BC \u09B9\u09CD\u09AF\u09BE\u09B6 \u09A8\u09BE\
  \u09AE\u09C7 \u09AA\u09B0\u09BF\u099A\u09BF\u09A4, \u0987\u0989\u09A8\u09BF\u0995\
  \ \u0995\u09C0 \u098F\u09AC\u0982 \u09AE\u09BE\u09A8\u0997\u09C1\u09B2\u09BF \u099C\
  \u09CB\u09A1\u09BC\u09BE \u09B2\u09BE\u0997\u09BE\u09A4\u09C7 \u09B8\u09BE\u09B9\
  \u09BE\u09AF\u09CD\u09AF \u0995\u09B0\u09C7\u0964 \u09AF\u0996\u09A8 \u0995\u09CB\
  \u09A8\u09CB \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09B0\u09C7\
  \u09AB\u09BE\u09B0\u09C7\u09A8\u09CD\u09B8\u09C7\u09B0\u2026"
lastmod: '2024-03-17T18:47:44.580063-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u09B8\u09CB\u09B8\u09BF\u09AF\u09BC\u09C7\u099F\u09BF\u09AD \u0985\
  \u09CD\u09AF\u09BE\u09B0\u09C7\u0997\u09C1\u09B2\u09BF, \u09AF\u09BE \u09B0\u09C1\
  \u09AC\u09BF \u09AD\u09BE\u09B7\u09BE\u09AF\u09BC \u0985\u09A7\u09BF\u0995\u09BE\
  \u0982\u09B6 \u09B8\u09AE\u09AF\u09BC \u09B9\u09CD\u09AF\u09BE\u09B6 \u09A8\u09BE\
  \u09AE\u09C7 \u09AA\u09B0\u09BF\u099A\u09BF\u09A4, \u0987\u0989\u09A8\u09BF\u0995\
  \ \u0995\u09C0 \u098F\u09AC\u0982 \u09AE\u09BE\u09A8\u0997\u09C1\u09B2\u09BF \u099C\
  \u09CB\u09A1\u09BC\u09BE \u09B2\u09BE\u0997\u09BE\u09A4\u09C7 \u09B8\u09BE\u09B9\
  \u09BE\u09AF\u09CD\u09AF \u0995\u09B0\u09C7\u0964 \u09AF\u0996\u09A8 \u0995\u09CB\
  \u09A8\u09CB \u09A8\u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09B0\u09C7\
  \u09AB\u09BE\u09B0\u09C7\u09A8\u09CD\u09B8\u09C7\u09B0\u2026"
title: "\u098F\u09B8\u09CB\u09B8\u09BF\u09AF\u09BC\u09C7\u099F\u09BF\u09AD \u0985\u09CD\
  \u09AF\u09BE\u09B0\u09C7\u09B0 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0"
---

{{< edit_this_page >}}

## কি এবং কেন?

এসোসিয়েটিভ অ্যারেগুলি, যা রুবি ভাষায় অধিকাংশ সময় হ্যাশ নামে পরিচিত, ইউনিক কী এবং মানগুলি জোড়া লাগাতে সাহায্য করে। যখন কোনো নির্দিষ্ট রেফারেন্সের মাধ্যমে উপাদানগুলি ট্র্যাক রাখতে হয়, যেমন একটি অবজেক্টের বৈশিষ্ট্যগুলি সংরক্ষণ করা বা একটি অনন্য পরিচিতিতে দ্রুত ডেটা অ্যাক্সেস করা, তখন হ্যাশ অপরিহার্য।

## কিভাবে:

রুবিতে হ্যাশ তৈরি করা এবং ব্যবহার করা সোজা সাপটা। আপনি একটি খালি হ্যাশ আরম্ভ করতে পারেন, তাতে কী-মান এর যুগল দিয়ে পূর্ণ করতে পারেন, তাদের কীগুলি দ্বারা মান অ্যাক্সেস করতে পারেন, এবং আরও অনেক কিছু। আপনি এটা কিভাবে করবেন তা এখানে:

```Ruby
# একটি হ্যাশ তৈরি করা
my_hash = { "name" => "John Doe", "age" => 30 }

# হ্যাশ তৈরির আরেকটি উপায়
another_hash = Hash.new
another_hash["position"] = "Developer"

# হ্যাশ মান অ্যাক্সেস করা
puts my_hash["name"] # আউটপুট: John Doe

# নতুন কী-মান যুগল যোগ করা
my_hash["language"] = "Ruby"
puts my_hash # আউটপুট: {"name"=>"John Doe", "age"=>30, "language"=>"Ruby"}

# একটি হ্যাশের মাধ্যমে পুনরাবৃত্তি
my_hash.each do |key, value|
  puts "#{key}: #{value}"
end
# আউটপুট:
# name: John Doe
# age: 30
# language: Ruby
```

আপনি আরও কার্যকরী কী হিসেবে প্রতীক ব্যবহার করতে পারেন:

```Ruby
# কীগুলির জন্য প্রতীক ব্যবহার
symbol_hash = { name: "Jane Doe", age: 22 }
puts symbol_hash[:name] # আউটপুট: Jane Doe
```

## গভীর ডুব:

এসোসিয়েটিভ অ্যারের ধারণা শুধুমাত্র রুবি-তে সীমাবদ্ধ নয়; অনেক ভাষায় নানা নামে এই ধারণাটি বাস্তবায়িত হয়, যেমন পাইথনে ডিকশনারি বা জাভাস্ক্রিপ্টে অবজেক্টস (যখন কী-মান জুটি হিসেবে ব্যবহৃত হয়)। রুবির প্রারম্ভিক দিকে, হ্যাশ কিছুটা ধীর এবং তেমন বহুমুখী ছিল না। তবে, সময়ের সাথে সাথে, রুবির হ্যাশ বাস্তবায়ন অত্যন্ত অপ্টিমাইজ করা হয়েছে, বিশেষত প্রতীক কীগুলির জন্য, যা তাদের প্রায়শই অ্যাক্সেস এবং আপডেটের জন্য অত্যন্ত কার্যকর করে তোলে।

রুবির হ্যাশ তাদের সিন্টাক্সের সহজবোধ্যতা এবং নমনীয়তার জন্য বিখ্যাত - আপনি প্রায় যেকোনো অবজেক্ট টাইপকে কী হিসেবে ব্যবহার করতে পারেন, যদিও প্রতীক ও স্ট্রিংগুলি সবচেয়ে সাধারণ। অভ্যন্তরীণভাবে, রুবি হ্যাশগুলি একটি হ্যাশিং অ্যালগরিদম ব্যবহার করে বাস্তবায়িত হয় যা গতি এবং মেমরি দক্ষতার মধ্যে সামঞ্জস্য বজায় রাখে, এমনকি উপাদানের সংখ্যা বৃদ্ধি পাওয়ার সাথে সাথেও।

যদিও হ্যাশগুলি অত্যন্ত বহুমুখী, তবে রুবির ডেটা স্টোরেজের জন্য এগুলি সর্বস্ব সমাধান নয়। অর্ডারড কালেকশনের জন্য, অ্যারেগুলি আরও উপযুক্ত, এবং অনন্য আইটেমের সেটের জন্য, একটি সেট আরও ভালো পছন্দ হতে পারে। উপরন্তু, খুবই জটিল ডেটা স্ট্রাকচারের জন্য, কাস্টম ক্লাস তৈরি করা পরামর্শযোগ্য হতে পারে।

মনে রাখবেন, হ্যাশ ব্যবহারের বনাম অন্যান্য ডেটা স্ট্রাকচারগুলির মধ্যে পছন্দের সিদ্ধান্ত মূলত নির্দিষ্ট ব্যবহারের ক্ষেত্রের উপর নির্ভর করে - হ্যাশগুলি দ্রুত লুকাপ এবং ইউনিক কীগুলি এবং তাদের মানের মধ্যে সম্পর্ক বজায় রাখার ক্ষেত্রে অসাধারণ।