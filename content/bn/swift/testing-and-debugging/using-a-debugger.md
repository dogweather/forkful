---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:23:01.844490-06:00
description: "\u0995\u09C0\u09AD\u09BE\u09AC\u09C7: Xcode-\u098F (Swift-\u098F\u09B0\
  \ IDE) \u09A1\u09BF\u09AC\u09BE\u0997\u09BE\u09B0 \u09AC\u09CD\u09AF\u09AC\u09B9\
  \u09BE\u09B0 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF, \u0986\u09AA\u09A8\
  \u09BF \u09AC\u09CD\u09B0\u09C7\u0995\u09AA\u09DF\u09C7\u09A8\u09CD\u099F \u09B8\
  \u09C7\u099F \u0995\u09B0\u09A4\u09C7, \u09AD\u09C7\u09B0\u09BF\u09DF\u09C7\u09AC\
  \u09B2\u09B8 \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\u09BE \u0995\u09B0\u09A4\u09C7\
  , \u098F\u09AC\u0982 \u09AE\u09A8\u09C7 \u09B0\u09BE\u0996\u09BE \u09AA\u09CD\u09B0\
  \u0995\u09BE\u09B6\u09A8\u09C7\u09B0 \u09A4\u09BE\u09B2\u09BF\u0995\u09BE\u09DF\
  \ \u098F\u0995\u09CD\u09B8\u09AA\u09CD\u09B0\u09C7\u09B6\u09A8\u2026"
lastmod: '2024-03-17T18:47:44.414225-06:00'
model: gpt-4-0125-preview
summary: "Xcode-\u098F (Swift-\u098F\u09B0 IDE) \u09A1\u09BF\u09AC\u09BE\u0997\u09BE\
  \u09B0 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09BE\u09B0 \u099C\
  \u09A8\u09CD\u09AF, \u0986\u09AA\u09A8\u09BF \u09AC\u09CD\u09B0\u09C7\u0995\u09AA\
  \u09DF\u09C7\u09A8\u09CD\u099F \u09B8\u09C7\u099F \u0995\u09B0\u09A4\u09C7, \u09AD\
  \u09C7\u09B0\u09BF\u09DF\u09C7\u09AC\u09B2\u09B8 \u09AA\u09B0\u09C0\u0995\u09CD\u09B7\
  \u09BE \u0995\u09B0\u09A4\u09C7, \u098F\u09AC\u0982 \u09AE\u09A8\u09C7 \u09B0\u09BE\
  \u0996\u09BE \u09AA\u09CD\u09B0\u0995\u09BE\u09B6\u09A8\u09C7\u09B0 \u09A4\u09BE\
  \u09B2\u09BF\u0995\u09BE\u09DF \u098F\u0995\u09CD\u09B8\u09AA\u09CD\u09B0\u09C7\u09B6\
  \u09A8 \u09AF\u09C1\u0995\u09CD\u09A4 \u0995\u09B0\u09A4\u09C7 \u09AA\u09BE\u09B0\
  \u09C7\u09A8\u0964 \u098F\u0995\u099F\u09BF \u0989\u09A6\u09BE\u09B9\u09B0\u09A3\
  \ \u09A6\u09C7\u0996\u09C1\u09A8."
title: "\u09A1\u09BF\u09AC\u09BE\u0997\u09BE\u09B0 \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09BE"
weight: 35
---

## কীভাবে:
Xcode-এ (Swift-এর IDE) ডিবাগার ব্যবহার করার জন্য, আপনি ব্রেকপয়েন্ট সেট করতে, ভেরিয়েবলস পরীক্ষা করতে, এবং মনে রাখা প্রকাশনের তালিকায় এক্সপ্রেশন যুক্ত করতে পারেন। একটি উদাহরণ দেখুন:

```Swift
func findFactorial(of number: Int) -> Int {
    if number == 0 {
        return 1
    }
    return number * findFactorial(of: number - 1)
}

let result = findFactorial(of: 5)
print(result)
```

Xcode-এ একটি লাইন নম্বরের বামে ক্লিক করে ব্রেকপয়েন্ট সেট করুন, এবং প্রোগ্রামটি চালান। যখন এটি ব্রেকপয়েন্টে পৌঁছাবে, Xcode নির্বাহ বিলম্বিত করে। এখন আপনি পারেন:

1. ভেরিয়েবলের মান চেক করুন।
2. ডিবাগার কন্ট্রোল ব্যবহার করে পরবর্তী লাইনে (step over) চালান অথবা ফাংশনের ভেতরে যান (step into)।
3. বিশেষ ভেরিয়েবল বা কনস্ট্যান্টসের পরিবর্তন মনিটর করতে 'watch list'-এ এক্সপ্রেশনগুলি যোগ করুন।

ডিবাগ এরিয়ায় যা দেখতে পারেন:

```
(lldb) po number
5
(lldb) po result
120
```

## গভীর ডুব:
ডিবাগারস ১৯৪০ এর দশক থেকে প্রোগ্রামিং ল্যান্ডস্কেপের একটি অংশ হয়ে উঠেছে, সাধারণ ব্রেকপয়েন্ট সিস্টেম থেকে শুরু করে জটিল, UI-চালিত অভিজ্ঞতায় পরিবর্তন হয়েছে। Xcode-এর বিল্ট-ইন ডিবাগার ছাড়াও অন্যান্য বিকল্পের মধ্যে LLDB (Low Level Debugger) রয়েছে যা Xcode অভ্যন্তরে ব্যবহার করে। কিছু মানুষ `print()` স্টেটমেন্ট দিয়ে ডিবাগিংও করে (স্নেহভাবে "caveman debugging" হিসেবে পরিচিত) তবে বড় প্রকল্পের জন্য বা জটিল বাগসের ক্ষেত্রে এটি কম কার্যকর। যখন আপনি ডিবাগার ব্যবহার করেন, আপনি নির্বাহের নিয়ন্ত্রণ, রানটাইম অন্তর্দৃষ্টি, এবং ডেটা পরিচালনা প্রক্রিয়াকে সামলাচ্ছেন। এই নীতিগুলির গভীর বোঝাপড়া দক্ষ ডিবাগিংয়ে অনেক দূর পর্যন্ত সাহায্য করে।

## আরও দেখুন:
- [অ্যাপলের Xcode ডিবাগিং গাইড](https://developer.apple.com/documentation/xcode/debugging/)
- [LLDB দ্রুত শুরুর গাইড](https://lldb.llvm.org/use/tutorial.html)
- [রে ওয়েন্ডার্লিচের সুইফ্‌ট ডিবাগিং টিউটোরিয়াল](https://www.raywenderlich.com/966538-arc-and-memory-management-in-swift)
