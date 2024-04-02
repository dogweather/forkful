---
changelog:
- 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-17 17:45:36.621944-06:00
description: "\u0995\u0996\u09A8\u09CB \u0995\u09BF \u0986\u09AA\u09A8\u09BE\u0995\
  \u09C7 \u0985\u09A4\u09C0\u09A4 \u09AC\u09BE \u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\
  \u09A4\u09C7\u09B0 \u098F\u0995\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996 \u0996\
  \u09C1\u0981\u099C\u09A4\u09C7 \u09B9\u09AF\u09BC\u09C7\u099B\u09C7? \u09AA\u09CD\
  \u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09C7\u099F\u09BF\u0995 \u09AD\u09BE\u09AC\
  \u09C7, \u0986\u09AE\u09B0\u09BE \u09AA\u09CD\u09B0\u09BE\u09AF\u09BC\u09B6\u0987\
  \ \u09AE\u09C7\u09AF\u09BC\u09BE\u09A6, \u0985\u09A8\u09C1\u09B8\u09CD\u09AE\u09BE\
  \u09B0\u0995, \u09AC\u09BE \u0987\u09AD\u09C7\u09A8\u09CD\u099F\u09C7\u09B0 \u099C\
  \u09A8\u09CD\u09AF \u09A4\u09BE\u09B0\u09BF\u0996 \u0997\u09A3\u09A8\u09BE \u0995\
  \u09B0\u09BF\u0964 \u0995\u09C0\u09AD\u09BE\u09AC\u09C7\u2026"
lastmod: '2024-03-17T18:47:44.423409-06:00'
model: gpt-4-0125-preview
summary: "\u0995\u0996\u09A8\u09CB \u0995\u09BF \u0986\u09AA\u09A8\u09BE\u0995\u09C7\
  \ \u0985\u09A4\u09C0\u09A4 \u09AC\u09BE \u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4\
  \u09C7\u09B0 \u098F\u0995\u099F\u09BF \u09A4\u09BE\u09B0\u09BF\u0996 \u0996\u09C1\
  \u0981\u099C\u09A4\u09C7 \u09B9\u09AF\u09BC\u09C7\u099B\u09C7? \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09C7\u099F\u09BF\u0995 \u09AD\u09BE\u09AC\u09C7\
  , \u0986\u09AE\u09B0\u09BE \u09AA\u09CD\u09B0\u09BE\u09AF\u09BC\u09B6\u0987 \u09AE\
  \u09C7\u09AF\u09BC\u09BE\u09A6, \u0985\u09A8\u09C1\u09B8\u09CD\u09AE\u09BE\u09B0\
  \u0995, \u09AC\u09BE \u0987\u09AD\u09C7\u09A8\u09CD\u099F\u09C7\u09B0 \u099C\u09A8\
  \u09CD\u09AF \u09A4\u09BE\u09B0\u09BF\u0996 \u0997\u09A3\u09A8\u09BE \u0995\u09B0\
  \u09BF\u0964 \u0995\u09C0\u09AD\u09BE\u09AC\u09C7\u2026"
title: "\u09AD\u09AC\u09BF\u09B7\u09CD\u09AF\u09A4 \u09AC\u09BE \u0985\u09A4\u09C0\
  \u09A4\u09C7\u09B0 \u09A4\u09BE\u09B0\u09BF\u0996 \u0997\u09A3\u09A8\u09BE \u0995\
  \u09B0\u09BE"
weight: 26
---

## কি এবং কেন?
কখনো কি আপনাকে অতীত বা ভবিষ্যতের একটি তারিখ খুঁজতে হয়েছে? প্রোগ্রামেটিক ভাবে, আমরা প্রায়শই মেয়াদ, অনুস্মারক, বা ইভেন্টের জন্য তারিখ গণনা করি। কীভাবে জানা অনুমানের বাইরে নিয়ে আসে এবং আপনার অ্যাপকে সময়-সংবেদনশীল কাজ সঠিক ভাবে সামাল দিতে দেয়।

## কিভাবে:
Swift এ `Calendar` এবং `DateComponents` এর মাধ্যমে তারিখের হিসাব সহজ করে দেয়। এখানে মূল ধারণা:

```Swift
import Foundation

// আজকের তারিখ
let today = Date()

// ব্যবহারকারীর বর্তমান ক্যালেন্ডার পেতে
let currentCalendar = Calendar.current

// আজকের থেকে ২ সপ্তাহ যোগ
if let twoWeeksLater = currentCalendar.date(byAdding: .weekOfYear, value: 2, to: today) {
    print("দুই সপ্তাহ পরে: \(twoWeeksLater)")
}

// আজকের থেকে ৩০ দিন বিয়োগ
if let thirtyDaysBefore = currentCalendar.date(byAdding: .day, value: -30, to: today) {
    print("ত্রিশ দিন আগে: \(thirtyDaysBefore)")
}
```
আউটপুট হতে পারে এরকম:
```
দুই সপ্তাহ পরে: 2023-04-14 10:26:47 +0000
ত্রিশ দিন আগে: 2023-03-15 10:26:47 +0000
```
মনে রাখবেন, প্রকৃত আউটপুট পরিবর্তনশীল হবে কারণ `Date()` আপনাকে বর্তমান তারিখ এবং সময় দেয়।

## গভীর ডুব
Swift এর আগে, Objective-C এবং এর জটিল সিনট্যাক্স প্রভাবশালী ছিল। Swift-এর `Date`, `Calendar`, এবং `DateComponents` তারিখের অপারেশনকে সরল করে। এই অবজেক্টগুলি সময় অঞ্চল, দিবালোক সঞ্চয় পরিবর্তন, এবং ব্যবহারকারীর ক্যালেন্ডার সেটিংস বিবেচনা করে – যা অবজেক্টিভ-সিতে সামাল দেওয়া একটি কঠিন কাজ ছিল।

বিকল্পগুলি মধ্যে সুইফটাইট এর মতো তৃতীয় পক্ষের লাইব্রেরিও রয়েছে, যা আরও বেশি সুবিধা এবং কার্যকারিতা প্রদান করতে পারে। কিন্তু অধিকাংশের জন্য, Swift-এর নিজস্ব টুলসগুলি সঠিকভাবে কাজ করে।

তারিখগুলি জটিল। এগুলি শুধুমাত্র বৃদ্ধি বা হ্রাস করার সংখ্যা নয়; এগুলি ক্যালেন্ডার, স্থানীয় বিশেষত্ব এবং সময় অঞ্চল জড়িত। অ্যাপলের ফাউন্ডেশন ফ্রেমওয়ার্ক এই জটিলতাকে সামলায়, নিশ্চিত করে যে আপনার ভবিষ্যত এবং অতীতের তারিখের হিসাব বিশ্বজুড়ে যুক্তিযুক্ত থাকে।
