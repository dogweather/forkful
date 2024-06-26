---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:11:44.821324-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u0986\u09B8\u09C1\u09A8 \u098F\
  \u0995\u099F\u09BF \u09B8\u09BE\u09A7\u09BE\u09B0\u09A3 Lua \u09AB\u09BE\u0982\u09B6\
  \u09A8 \u09A8\u09BF\u09AF\u09BC\u09C7 \u09B0\u09BF\u09AB\u09CD\u09AF\u09BE\u0995\
  \u09CD\u099F\u09B0 \u0995\u09B0\u09BF\u0964 \u0986\u09AE\u09B0\u09BE \u098F\u0995\
  \u099F\u09BF \u09AB\u09BE\u0982\u09B6\u09A8 \u09A6\u09BF\u09AF\u09BC\u09C7 \u09B6\
  \u09C1\u09B0\u09C1 \u0995\u09B0\u09BF \u09AF\u09BE \u098F\u0995\u099F\u09BF \u09A4\
  \u09BE\u09B2\u09BF\u0995\u09BE\u09AF\u09BC \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u0997\
  \u09C1\u09B2\u09BF\u09B0 \u09B8\u09AE\u09CD\u09AE\u09BF\u09B2\u09A8 \u0997\u09A3\
  \u09A8\u09BE \u0995\u09B0\u09C7 \u0995\u09BF\u09A8\u09CD\u09A4\u09C1 \u09A6\u0995\
  \u09CD\u09B7\u09A4\u09BE \u09AC\u09BE\u2026"
lastmod: '2024-03-17T18:47:44.188058-06:00'
model: gpt-4-0125-preview
summary: "\u0986\u09B8\u09C1\u09A8 \u098F\u0995\u099F\u09BF \u09B8\u09BE\u09A7\u09BE\
  \u09B0\u09A3 Lua \u09AB\u09BE\u0982\u09B6\u09A8 \u09A8\u09BF\u09AF\u09BC\u09C7 \u09B0\
  \u09BF\u09AB\u09CD\u09AF\u09BE\u0995\u09CD\u099F\u09B0 \u0995\u09B0\u09BF\u0964\
  \ \u0986\u09AE\u09B0\u09BE \u098F\u0995\u099F\u09BF \u09AB\u09BE\u0982\u09B6\u09A8\
  \ \u09A6\u09BF\u09AF\u09BC\u09C7 \u09B6\u09C1\u09B0\u09C1 \u0995\u09B0\u09BF \u09AF\
  \u09BE \u098F\u0995\u099F\u09BF \u09A4\u09BE\u09B2\u09BF\u0995\u09BE\u09AF\u09BC\
  \ \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u0997\u09C1\u09B2\u09BF\u09B0 \u09B8\u09AE\
  \u09CD\u09AE\u09BF\u09B2\u09A8 \u0997\u09A3\u09A8\u09BE \u0995\u09B0\u09C7 \u0995\
  \u09BF\u09A8\u09CD\u09A4\u09C1 \u09A6\u0995\u09CD\u09B7\u09A4\u09BE \u09AC\u09BE\
  \ \u09B8\u09CD\u09AA\u09B7\u09CD\u099F\u09A4\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF\
  \ \u0985\u09A8\u09C7\u0995 \u099A\u09BF\u09A8\u09CD\u09A4\u09BE \u09AC\u09BF\u09A8\
  \u09BE \u09B2\u09BF\u0996\u09BF\u09A4."
title: "\u09B0\u09BF\u09AB\u09CD\u09AF\u09BE\u0995\u09CD\u099F\u09B0\u09BF\u0982"
weight: 19
---

## কিভাবে:
আসুন একটি সাধারণ Lua ফাংশন নিয়ে রিফ্যাক্টর করি। আমরা একটি ফাংশন দিয়ে শুরু করি যা একটি তালিকায় সংখ্যাগুলির সম্মিলন গণনা করে কিন্তু দক্ষতা বা স্পষ্টতার জন্য অনেক চিন্তা বিনা লিখিত:

```Lua
function sumList(numbers)
    local result = 0
    for i=1, #numbers do
        for j=1, #numbers do
            if i == j then
                result = result + numbers[i]
            end
        end
    end
    return result
end

print(sumList({1, 2, 3, 4})) -- আউটপুট: 10
```

রিফ্যাক্টর করে আরো দক্ষ এবং পঠনযোগ্য ভার্সনে:
```Lua
function sumListRefactored(numbers)
    local result = 0
    for _, value in ipairs(numbers) do
        result = result + value
    end
    return result
end

print(sumListRefactored({1, 2, 3, 4})) -- এখনো আউটপুট: 10
```

রিফ্যাক্টরকৃত ভার্সনটি অপ্রয়োজনীয় অভ্যন্তরীণ লুপটি দূর করে, `ipairs` ব্যবহার করে পরিষ্কারভাবে তালিকা দ্বারা পুনরাবৃত্তি করে।

## গভীর ডুব:
ঐতিহাসিকভাবে, রিফ্যাক্টরিং শব্দটি আসে ৮০ এর দশকের শেষের Smalltalk প্রোগ্রামিং সম্প্রদায় থেকে এবং মার্টিন ফাউলারের বই 'Refactoring: Improving the Design of Existing Code' দ্বারা জনপ্রিয় হয়। Lua-তে, রিফ্যাক্টরিং প্রায়ই জটিল শর্তাবলী সরলীকরণ, বড় ফাংশনগুলিকে ছোট ছোট অংশে ভাঙ্গা এবং টেবিল ব্যবহারের অনুকূলন জড়িত।

Lua-তে রিফ্যাক্টরিং এর নিজস্ব সতর্কতাগুলি রয়েছে; Lua-র গতিশীল প্রকৃতি এবং নমনীয় টাইপিং নির্দিষ্ট রিফ্যাক্টরগুলি, যেমন ভেরিয়েবল পুনঃনামকরণ অথবা ফাংশন সিগনেচার পরিবর্তন, যদি সাবধানে না করা হয় তবে ঝুঁকিপূর্ণ করে তুলতে পারে। স্ট্যাটিক কোড বিশ্লেষণের টুলগুলি (যেমন `luacheck`) এরকম ঝুঁকিগুলি কমাতে পারে। বিকল্পগুলির মধ্যে টেস্ট-চালিত উন্নয়ন (TDD) অন্তর্ভুক্ত, যেখানে কোড উন্নয়নের প্রক্রিয়ার অভিন্ন অংশ হিসেবে ক্রমাগত রিফ্যাক্টর করা হয়, পৃথক রিফ্যাক্টরিং ধাপের বিপরীতে।

## আরো দেখুন
- "Programming in Lua" বইটি রবার্তো ইয়েরুসালিমস্কি দ্বারা, সেরা অনুশীলন এবং উদাহরণের জন্য।
- "Refactoring: Improving the Design of Existing Code" বইটি মার্টিন ফাউলার দ্বারা, ভাষাগুলি জুড়ে প্রযোজ্য নীতিগুলির জন্য।
- LuaRocks ডিরেক্টরি (https://luarocks.org/) Lua কোড বজায় রাখা এবং রিফ্যাক্টর করার উদ্দেশ্যে টুল এবং মডিউলগুলির জন্য।
