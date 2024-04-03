---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:38:57.107725-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Lua \u09A4\u09C7, \u0986\u09AA\
  \u09A8\u09BF \u099F\u09C7\u09AC\u09BF\u09B2\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\
  \u09AF\u09AE\u09C7 \u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE\
  \ \u09AA\u09CD\u09B0\u0995\u09BE\u09B6 \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\
  \u09C7\u09A8\u0964 \u09AE\u09CC\u09B2\u09BF\u0995 \u0985\u09AA\u09BE\u09B0\u09C7\
  \u09B6\u09A8\u0997\u09C1\u09B2\u09BF \u09B9\u09B2 \u098F\u0987 \u099F\u09C7\u09AC\
  \u09BF\u09B2\u0997\u09C1\u09B2\u09BF\u0995\u09C7 \u09AF\u09CB\u0997, \u09AC\u09BF\
  \u09AF\u09BC\u09CB\u0997, \u0997\u09C1\u09A3, \u098F\u09AC\u0982 \u09AD\u09BE\u0997\
  \ \u0995\u09B0\u09BE\u0964 \u098F\u0996\u09BE\u09A8\u09C7 \u0995\u09BF\u09AD\u09BE\
  \u09AC\u09C7."
lastmod: '2024-03-17T18:47:44.172772-06:00'
model: gpt-4-0125-preview
summary: "Lua \u09A4\u09C7, \u0986\u09AA\u09A8\u09BF \u099F\u09C7\u09AC\u09BF\u09B2\
  \u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u099C\u099F\u09BF\u09B2\
  \ \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u09AA\u09CD\u09B0\u0995\u09BE\u09B6 \u0995\
  \u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\u0964 \u09AE\u09CC\u09B2\u09BF\
  \u0995 \u0985\u09AA\u09BE\u09B0\u09C7\u09B6\u09A8\u0997\u09C1\u09B2\u09BF \u09B9\
  \u09B2 \u098F\u0987 \u099F\u09C7\u09AC\u09BF\u09B2\u0997\u09C1\u09B2\u09BF\u0995\
  \u09C7 \u09AF\u09CB\u0997, \u09AC\u09BF\u09AF\u09BC\u09CB\u0997, \u0997\u09C1\u09A3\
  , \u098F\u09AC\u0982 \u09AD\u09BE\u0997 \u0995\u09B0\u09BE\u0964 \u098F\u0996\u09BE\
  \u09A8\u09C7 \u0995\u09BF\u09AD\u09BE\u09AC\u09C7."
title: "\u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u09B0 \u09B8\
  \u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
weight: 14
---

## কিভাবে:
Lua তে, আপনি টেবিলের মাধ্যমে জটিল সংখ্যা প্রকাশ করতে পারেন। মৌলিক অপারেশনগুলি হল এই টেবিলগুলিকে যোগ, বিয়োগ, গুণ, এবং ভাগ করা। এখানে কিভাবে:

```lua
-- দুটি জটিল সংখ্যা টেবিল হিসাবে সংজ্ঞায়িত
local complex_a = { real = 3, imag = 5 }
local complex_b = { real = 2, imag = -4 }

-- দুটি জটিল সংখ্যা যোগ করার ফাংশন
local function add_complex(a, b)
  return { real = a.real + b.real, imag = a.imag + b.imag }
end

-- নমুনা আউটপুট
print(add_complex(complex_a, complex_b))  -- { real = 5, imag = 1 }
```

## গভীর ডুব
১৬তম শতাব্দী থেকে, জটিল সংখ্যা, যেসব সমীকরণ শুধু বাস্তব সংখ্যা দিয়ে সমাধান করা যায়নি তা সমাধানে সাহায্য করে আসছে। Lua নিজে একটি বিল্ট-ইন জটিল সংখ্যা প্রকার নেই। তবে, এটি কোনো সমস্যা নয়—আপনি উপরে দেখানো টেবিল এবং ফাংশন ব্যবহার করে নিজের জটিল সংখ্যার ম্যানিপুলেশন তৈরি করতে পারেন। অথবা, আপনার যদি আরও গভীর প্রয়োজন হয়, তাহলে LuaComplex মতো লাইব্রেরি ব্যবহার করুন। এটি একটি সুন্দর পছন্দ কারণ এটি নির্দিষ্টভাবে Lua এর জন্য তৈরি এবং আপনার কাজকে সহজ করে দেয়। এরকম লাইব্রেরিগুলো প্রায়ই অপারেশনগুলি অদৃশ্যভাবে অপ্টিমাইজ করে যাতে এগুলি নিজের তৈরির চেয়ে দ্রুত হয়।

## দেখুন এছাড়াও
আরও বিস্তারিত উদাহরণ এবং উন্নত অপারেশনের জন্য, এগুলো দেখুন:

- LuaComplex লাইব্রেরি: https://github.com/davidm/lua-complex
- "Programming in Lua" বই, কাস্টম ডাটা প্রকার তৈরির জন্য: https://www.lua.org/pil/11.1.html
- বিভিন্ন ক্ষেত্রে জটিল সংখ্যার ব্যবহার সম্পর্কিত উইকিপিডিয়া: https://en.wikipedia.org/wiki/Complex_number#Applications
