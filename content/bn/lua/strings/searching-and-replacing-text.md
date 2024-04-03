---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:16:28.492341-06:00
description: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u0985\u09A8\u09C1\u09B8\u09A8\u09CD\
  \u09A7\u09BE\u09A8 \u098F\u09AC\u0982 \u09AA\u09CD\u09B0\u09A4\u09BF\u09B8\u09CD\
  \u09A5\u09BE\u09AA\u09A8 \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2 \u098F\u0995\u099F\
  \u09BF \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AC\u09CD\u09B2\u0995\u09C7 \u09A8\
  \u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u0985\u09A8\u09CD\u09AF\u09C7\u09B0\
  \ \u09B8\u09BE\u09A5\u09C7 \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8 \u0995\
  \u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\
  \u09B0\u09BE \u098F\u099F\u09BE \u09A4\u09CD\u09B0\u09C1\u099F\u09BF \u09B8\u0982\
  \u09B6\u09CB\u09A7\u09A8, \u09A4\u09A5\u09CD\u09AF \u0986\u09AA\u09A1\u09C7\u099F\
  \u2026"
lastmod: '2024-03-17T18:47:44.162022-06:00'
model: gpt-4-0125-preview
summary: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u0985\u09A8\u09C1\u09B8\u09A8\u09CD\
  \u09A7\u09BE\u09A8 \u098F\u09AC\u0982 \u09AA\u09CD\u09B0\u09A4\u09BF\u09B8\u09CD\
  \u09A5\u09BE\u09AA\u09A8 \u09AE\u09BE\u09A8\u09C7 \u09B9\u09B2 \u098F\u0995\u099F\
  \u09BF \u099F\u09C7\u0995\u09CD\u09B8\u099F \u09AC\u09CD\u09B2\u0995\u09C7 \u09A8\
  \u09BF\u09B0\u09CD\u09A6\u09BF\u09B7\u09CD\u099F \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u0985\u09A8\u09CD\u09AF\u09C7\u09B0\
  \ \u09B8\u09BE\u09A5\u09C7 \u09AA\u09B0\u09BF\u09AC\u09B0\u09CD\u09A4\u09A8 \u0995\
  \u09B0\u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\
  \u09B0\u09BE \u098F\u099F\u09BE \u09A4\u09CD\u09B0\u09C1\u099F\u09BF \u09B8\u0982\
  \u09B6\u09CB\u09A7\u09A8, \u09A4\u09A5\u09CD\u09AF \u0986\u09AA\u09A1\u09C7\u099F\
  \ \u0995\u09B0\u09BE, \u09AC\u09BE \u09A1\u09BE\u099F\u09BE \u09AB\u09B0\u09AE\u09CD\
  \u09AF\u09BE\u099F\u09BF\u0982 \u098F\u09B0 \u09AE\u09A4\u09CB \u0995\u09BE\u099C\
  \u09C7 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09A5\u09BE\
  \u0995\u09C7\u0964."
title: "\u099F\u09C7\u0995\u09CD\u09B8\u099F \u0985\u09A8\u09C1\u09B8\u09A8\u09CD\u09A7\
  \u09BE\u09A8 \u098F\u09AC\u0982 \u09AA\u09CD\u09B0\u09A4\u09BF\u09B8\u09CD\u09A5\
  \u09BE\u09AA\u09A8"
weight: 10
---

## কিভাবে:
Lua-র `string.gsub` ফাংশন হল অনুসন্ধান এবং প্রতিস্থাপনের জন্য আপনার আদর্শ বিকল্প। এটি এভাবে কাজ করে:

```lua
local text = "The quick brown fox jumps over the lazy dog."
local searchText = "lazy"
local replaceWith = "energetic"

local result = string.gsub(text, searchText, replaceWith)

print(result)
```

আউটপুট:

```
The quick brown fox jumps over the energetic dog.
```

সকল ঘটনা প্রতিস্থাপনের জন্য, `gsub` এটি ডিফল্ট হিসেবে করে:

```lua
local text = "Apples are sweet. Apples are juicy."
local result = string.gsub(text, "Apples", "Oranges")

print(result)
```

আউটপুট:

```
Oranges are sweet. Oranges are juicy.
```

## গভীর ডুব:
টেক্সট অনুসন্ধান এবং প্রতিস্থাপন লুয়া-তে একচেটিয়া নয়; এটি প্রোগ্রামিং ভাষাগুলিতে একটি সাধারণ বৈশিষ্ট্য। লুয়ার `string.gsub` তার স্ট্রিং ম্যানিপুলেশন মূলে ফিরে আসে, প্যাটার্ন এবং প্রতিস্থাপন সম্পর্কে একটি সরল প্রক্রিয়া প্রস্তাব করে।

ঐতিহাসিকভাবে, `gsub` (গ্লোবাল প্রতিস্থাপন) ইউনিক্সের `sed` কমান্ড এবং পার্লের শক্তিশালী প্যাটার্ন মিলান ক্ষমতার দ্বারা প্রভাবিত। লুয়ার প্যাটার্ন, অন্যান্য ভাষায় পাওয়া নিয়মিত অভিব্যক্তিগুলির চেয়ে সাধারণ, তবুও একটু সৃজনশীলতার সাথে জটিল ম্যাচগুলি সামলাতে পারে।

`string.gsub` এর বিকল্পগুলি ম্যানুয়ালি স্ট্রিংগুলির মাধ্যমে ইটারেট করা এবং প্রতিস্থাপন তৈরি করা অন্তর্ভুক্ত—একটি বেশি ভুলপ্রবণ পদ্ধতি। ভারী টেক্সট প্রক্রিয়াকরণের জন্য, উৎসর্গীকৃত পার্সিং লাইব্রেরিগুলি ব্যবহার করা হতে পারে।

কার্যকারিতার দিক থেকে, `gsub` প্রতিস্থাপন যুক্তিতে একটি ফাংশন নেওয়ার সুযোগ দিতে পারে যা প্রতিস্থাপনের উপর প্রোগ্রামযোগ্য নিয়ন্ত্রণ অনুমতি দেয়।

```lua
local result = string.gsub(text, "(%a+)", function(word)
  return #word > 4 and word:upper() or word
end)
```

এই স্নিপেট চারের অধিক অক্ষরের শব্দগুলিকে বড় হাতের অক্ষরে প্রকাশ করবে।

## আরও দেখুন
- [প্রোগ্রামিং ইন লুয়া বই](https://www.lua.org/pil/), লুয়ার প্রোগ্রামিং ধারণাগুলির গভীর জ্ঞান প্রদান করে।
- লুয়ার পুর্ণ স্ট্রিং প্যাটার্ন ক্ষমতার জন্য, [লুয়া 5.4 রেফারেন্স ম্যানুয়াল](https://www.lua.org/manual/5.4/manual.html#6.4.1) পরীক্ষা করুন।
