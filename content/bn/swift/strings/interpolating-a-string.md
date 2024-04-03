---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 17:51:00.144843-06:00
description: "\u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u09AF\
  \u09BC\u09C7 \u0987\u09A8\u09CD\u099F\u09BE\u09B0\u09AA\u09CB\u09B2\u09C7\u09B6\u09A8\
  \ \u09AE\u09BE\u09A8\u09C7 \u09B9\u099A\u09CD\u099B\u09C7 \u098F\u0995\u099F\u09BF\
  \ \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09B2\u09BF\u099F\u09BE\u09B0\u09BE\
  \u09B2\u09C7 \u09AD\u09C7\u09B0\u09BF\u09AF\u09BC\u09C7\u09AC\u09B2 \u09AA\u09CD\
  \u09B0\u09AC\u09C7\u09B6 \u0995\u09B0\u09BE\u09A8\u09CB\u0964 \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09B8\u09CD\u099F\u09CD\
  \u09B0\u09BF\u0982 \u0997\u09C1\u09B2\u09CB\u0995\u09C7 \u09A1\u09BE\u0987\u09A8\
  \u09BE\u09AE\u09BF\u0995\u09AD\u09BE\u09AC\u09C7 \u09A4\u09C8\u09B0\u09BF \u0995\
  \u09B0\u09A4\u09C7 \u098F\u099F\u09BF \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09C7\
  \u09A8,\u2026"
lastmod: '2024-03-17T18:47:44.394688-06:00'
model: gpt-4-0125-preview
summary: "\u098F\u0995\u099F\u09BF \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\u09AF\
  \u09BC\u09C7 \u0987\u09A8\u09CD\u099F\u09BE\u09B0\u09AA\u09CB\u09B2\u09C7\u09B6\u09A8\
  \ \u09AE\u09BE\u09A8\u09C7 \u09B9\u099A\u09CD\u099B\u09C7 \u098F\u0995\u099F\u09BF\
  \ \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09B2\u09BF\u099F\u09BE\u09B0\u09BE\
  \u09B2\u09C7 \u09AD\u09C7\u09B0\u09BF\u09AF\u09BC\u09C7\u09AC\u09B2 \u09AA\u09CD\
  \u09B0\u09AC\u09C7\u09B6 \u0995\u09B0\u09BE\u09A8\u09CB\u0964 \u09AA\u09CD\u09B0\
  \u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE \u09B8\u09CD\u099F\u09CD\
  \u09B0\u09BF\u0982 \u0997\u09C1\u09B2\u09CB\u0995\u09C7 \u09A1\u09BE\u0987\u09A8\
  \u09BE\u09AE\u09BF\u0995\u09AD\u09BE\u09AC\u09C7 \u09A4\u09C8\u09B0\u09BF \u0995\
  \u09B0\u09A4\u09C7 \u098F\u099F\u09BF \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09C7\
  \u09A8, \u09AF\u09BE\u09A4\u09C7 \u0995\u09B0\u09C7 \u0986\u0989\u099F\u09AA\u09C1\
  \u099F\u09C7 \u09AD\u09C7\u09B0\u09BF\u09AF\u09BC\u09C7\u09AC\u09B2 \u09A1\u09BE\
  \u099F\u09BE \u0985\u09A8\u09CD\u09A4\u09B0\u09CD\u09AD\u09C1\u0995\u09CD\u09A4\
  \ \u0995\u09B0\u09BE \u09B8\u09B9\u099C \u09B9\u09AF\u09BC\u0964."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u0987\u09A8\u09CD\u099F\u09BE\u09B0\
  \u09AA\u09CB\u09B2\u09C7\u099F \u0995\u09B0\u09BE"
weight: 8
---

## কিভাবে:
Swift এ স্ট্রিং ইন্টারপোলেশনকে `\(variableName)` সিনট্যাক্সের মাধ্যমে খুব সহজে করা যায়।

```Swift
let name = "Jane"
let age = 28
let greeting = "Hello, \(name), you are \(age) years old."
print(greeting)  // আউটপুট: Hello, Jane, you are 28 years old.
```

আপনি ইন্টারপোলেশনের মধ্যে অপারেশনও করতে পারেন:

```Swift
let apples = 3
let oranges = 5
let fruitSummary = "I have \(apples + oranges) pieces of fruit."
print(fruitSummary)  // আউটপুট: I have 8 pieces of fruit.
```

## গভীর ডাইভ
চলুন একটু ইতিহাস ঘাঁটি করি। স্ট্রিং ইন্টারপোলেশন শুধুমাত্র Swift এ নেই; এটি অনেক ভাষায় বিদ্যমান (যেমন জাভাস্ক্রিপ্ট, পাইথন, ইত্যাদি)। তবে Swift এর সংস্করণটি টাইপ-সেফ, অর্থাৎ কম্পাইলার আপনার হয়ে টাইপ চেক করে, যা ত্রুটি হ্রাস করে।

Swift 5 এর আগে, স্ট্রিং ইন্টারপোলেশন কম শক্তিশালী এবং বেশি বিরক্তিকর ছিল। কিন্তু Swift 5 এ এক্সটেন্ডেড স্ট্রিং ইন্টারপোলেশন চালু করা হয়েছে, যা আপনাকে স্ট্রিং ইন্টারপোলেশন কাস্টমাইজ করতে দেয়, এবং এতে অসাধারণ নমনীয়তা আনা হয়।

Swift এ স্ট্রিং ইন্টারপোলেশনের বিকল্পগুলোতে `+` সংযোজন এবং পুরানো `String(format:)` পদ্ধতি অন্তর্ভুক্ত। যাইহোক, এগুলি কম সুবিধাজনক এবং, ফরম্যাট স্ট্রিংগুল৤ পড়া অধিক কঠিন।

বাস্তবায়নের বিস্তারিত? Swift এর স্ট্রিং ইন্টারপোলেশনে, আপনি স্ট্রিংয়ের মধ্যে ধরনের প্রতিনিধিত্ব কাস্টমাইজ করতে `StringInterpolation` প্রটোকল এক্সটেন্ড করে পারেন। অর্থাৎ আপনি ইন্টারপোলেশনের সময় কাস্টম প্রকারগুলি কীভাবে প্রদর্শিত হবে তা সংজ্ঞায়িত করতে পারেন, যা খুবই সহায়ক।

```Swift
extension String.StringInterpolation {
    mutating func appendInterpolation(_ value: Date) {
        let formatter = DateFormatter()
        formatter.dateStyle = .medium
        appendLiteral(formatter.string(from: value))
    }
}

let today = Date()
let dateString = "Today's date is \(today)."
print(dateString) // আউটপুট আজকের তারিখ মিডিয়াম স্টাইল ফর্ম্যাটিং এ হবে।
```

## আরও দেখুন
স্ট্রিং ইন্টারপোলেশন সম্পর্কে আরও তথ্যের জন্য, Swift এর ডকুমেন্টেশন খুবই উপকারী:
- [String Interpolation](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID292)
- [Improved String Interpolation এর জন্য Swift Evolution Proposal](https://github.com/apple/swift-evolution/blob/main/proposals/0228-fix-expressiblebystringinterpolation.md)

কাস্টম প্রকারগুলি ফর্ম্যাট করা নিয়ে গভীরে ডুব দিতে:
- [Swift এ স্ট্রিং ইন্টারপোলেশন কাস্টমাইজিং](https://www.hackingwithswift.com/articles/178/super-powered-string-interpolation-in-swift-5)
