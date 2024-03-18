---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:25:55.101005-06:00
description: "\u098F\u09B8\u09CB\u09B8\u09BF\u09AF\u09BC\u09C7\u099F\u09BF\u09AD \u0985\
  \u09CD\u09AF\u09BE\u09B0\u09C7\u0997\u09C1\u09B2\u09BF \u09B2\u09C1\u09AF\u09BC\u09BE\
  \u09A4\u09C7 \u09A1\u09C7\u099F\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u0997\u09CB\
  \u09AA\u09A8 \u09B9\u09BE\u09A4\u09AE\u09BF\u09B2\u09BE\u09A8 \u2014 \u09B6\u09C1\
  \u09A7\u09C1\u09AE\u09BE\u09A4\u09CD\u09B0 \u09B8\u0982\u0996\u09CD\u09AF\u09BE\
  \ \u09A6\u09CD\u09AC\u09BE\u09B0\u09BE \u0987\u09A8\u09A1\u09C7\u0995\u09CD\u09B8\
  \u09BF\u0982 \u09A8\u09AF\u09BC, \u0986\u09AA\u09A8\u09BE\u09B0 \u099A\u09BE\u09AC\
  \u09BF\u0997\u09C1\u09B2\u09BF \u09AF\u09BE \u0996\u09C1\u09B6\u09BF \u09A4\u09BE\
  \ \u09B9\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7, \u09AF\u09BE \u09A1\u09C7\u099F\u09BE\
  \u2026"
lastmod: '2024-03-17T18:47:44.171724-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u09B8\u09CB\u09B8\u09BF\u09AF\u09BC\u09C7\u099F\u09BF\u09AD \u0985\
  \u09CD\u09AF\u09BE\u09B0\u09C7\u0997\u09C1\u09B2\u09BF \u09B2\u09C1\u09AF\u09BC\u09BE\
  \u09A4\u09C7 \u09A1\u09C7\u099F\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u0997\u09CB\
  \u09AA\u09A8 \u09B9\u09BE\u09A4\u09AE\u09BF\u09B2\u09BE\u09A8 \u2014 \u09B6\u09C1\
  \u09A7\u09C1\u09AE\u09BE\u09A4\u09CD\u09B0 \u09B8\u0982\u0996\u09CD\u09AF\u09BE\
  \ \u09A6\u09CD\u09AC\u09BE\u09B0\u09BE \u0987\u09A8\u09A1\u09C7\u0995\u09CD\u09B8\
  \u09BF\u0982 \u09A8\u09AF\u09BC, \u0986\u09AA\u09A8\u09BE\u09B0 \u099A\u09BE\u09AC\
  \u09BF\u0997\u09C1\u09B2\u09BF \u09AF\u09BE \u0996\u09C1\u09B6\u09BF \u09A4\u09BE\
  \ \u09B9\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7, \u09AF\u09BE \u09A1\u09C7\u099F\u09BE\
  \u2026"
title: "\u098F\u09B8\u09CB\u09B8\u09BF\u09AF\u09BC\u09C7\u099F\u09BF\u09AD \u0985\u09CD\
  \u09AF\u09BE\u09B0\u09C7\u09B0 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0"
---

{{< edit_this_page >}}

## কি এবং কেন?

এসোসিয়েটিভ অ্যারেগুলি লুয়াতে ডেটার জন্য গোপন হাতমিলান — শুধুমাত্র সংখ্যা দ্বারা ইনডেক্সিং নয়, আপনার চাবিগুলি যা খুশি তা হতে পারে, যা ডেটা পুনঃপ্রাপ্তিকে সহজ করে তোলে। প্রোগ্রামাররা এগুলি কেন ব্যবহার করে? কারণ কখনও কখনও, আপনাকে ডেটাকে তার নামে ডাকতে হবে, একটি লাইনআপ নম্বর দ্বারা নয়।

## কিভাবে:

লুয়াতে, একটি এসোসিয়েটিভ অ্যারে (বা একটি টেবিল, লুয়া-ভাষায়) তৈরি করা সহজ। আপনি সাধারণ সংখ্যাত্মক ইন্ডেক্সগুলি পরিত্যাগ করে আপনার নিজের পছন্দের কীগুলি নির্বাচন করেন। এটা দেখুন:

```Lua
-- একটি এসোসিয়েটিভ অ্যারে তৈরি
userInfo = {
  name = "Jamie",
  occupation = "Adventurer",
  level = 42
}

-- উপাদানগুলি অ্যাক্সেস করা
print(userInfo["name"]) -- মুদ্রণ করে Jamie
print(userInfo.occupation) -- মুদ্রণ করে Adventurer

-- নতুন কী-মূল্য জোড়া যোগ করা
userInfo["hobby"] = "Coding"
userInfo.favLang = "Lua"

-- এসোসিয়েটিভ অ্যারের উপর ইটারেট করা
for key, value in pairs(userInfo) do
  print(key .. ": " .. value)
end
```

আউটপুট:
```
Jamie
Adventurer
name: Jamie
occupation: Adventurer
level: 42
hobby: Coding
favLang: Lua
```

মজার বিষয়? আপনি ডেটার সাথে যে এইসব চাবিগুলির অর্থপূর্ণ মাধ্যমে মিথস্ক্রিয়া করেন, যা কোডকে আরো পাঠযোগ্য এবং রক্ষণাবেক্ষণযোগ্য করে তোলে।

## গভীর ডুব

যখন লুয়া দৃশ্যপটে প্রবেশ করল, এটি টেবিলকে এক সর্বজনীন ডেটা কাঠামো হিসেবে চালু করে, যা ডেভেলপারদের ডেটা পরিচালনা করার উপায়ে বিপ্লব ঘটায়। কিছু ভাষায় যেখানে এসোসিয়েটিভ অ্যারেগুলি এবং অ্যারেগুলি পৃথক এন্টিটি হিসাবে বিবেচিত হয়, সেখানে লুয়ার টেবিল উভয় হিসাবে কাজ করে, ডেটা কাঠামো পরিদৃশ্যকে সরলীকৃত করে।

লুয়া টেবিল বিশেষত শক্তিশালী করে তোলে তাদের নমনীয়তা। তবে, এই নমনীয়তার সাথে বিশেষ করে বড় ডেটাসেটের ক্ষেত্রে সম্ভাব্য পারফরম্যান্সের প্রভাব আসে, যেখানে দক্ষতার জন্য একটি আরও বিশেষায়িত ডেটা কাঠামো পছন্দনীয় হতে পারে।

লুয়া সরাসরি আরও প্রচলিত ডেটা কাঠামোগুলি যেমন লিংকড লিস্ট বা হ্যাশ ম্যাপগুলির সমর্থন না করলেও, টেবিল কাঠামোর অভিযোজনযোগ্যতা মানে এই যে আপনি যদি প্রয়োজন হয় তাদের টেবিল ব্যবহার করে বাস্তবায়ন করতে পারেন। শুধু মনে রাখবেন: বড় ক্ষমতার সাথে বড় দায়িত্ব আসে। আপনার কোডের পারফরম্যান্স এবং পাঠযোগ্যতা বজায় রাখতে নমনীয়তাকে বুদ্ধিমত্তার সাথে ব্যবহার করুন।