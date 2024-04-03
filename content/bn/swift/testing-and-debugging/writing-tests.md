---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:42:03.334655-06:00
description: "\u0995\u09C0\u09AD\u09BE\u09AC\u09C7: Swift \u098F\u09B0 XCTest \u09AB\
  \u09CD\u09B0\u09C7\u09AE\u0993\u09AF\u09BC\u09BE\u09B0\u09CD\u0995\u09C7\u09B0 \u09AE\
  \u09BE\u09A7\u09CD\u09AF\u09AE\u09C7 \u099F\u09C7\u09B8\u09CD\u099F\u09BF\u0982\
  \ \u09B8\u09AE\u09B0\u09CD\u09A5\u09A8 \u0995\u09B0\u09C7, \u09AF\u09BE Xcode \u098F\
  \ \u09B8\u09AE\u09A8\u09CD\u09AC\u09BF\u09A4 \u09B9\u09AF\u09BC\u09C7\u099B\u09C7\
  \u0964 \u0986\u09AA\u09A8\u09BF \u0986\u09AA\u09A8\u09BE\u09B0 \u0995\u09CB\u09A1\
  \u09C7\u09B0 \u09AA\u09CD\u09B0\u09A4\u09BF\u099F\u09BF \u0985\u0982\u09B6 \u09AF\
  \u09BE\u099A\u09BE\u0987 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u0987\
  \u0989\u09A8\u09BF\u099F \u099F\u09C7\u09B8\u09CD\u099F\u2026"
lastmod: '2024-03-17T18:47:44.413253-06:00'
model: gpt-4-0125-preview
summary: "Swift \u098F\u09B0 XCTest \u09AB\u09CD\u09B0\u09C7\u09AE\u0993\u09AF\u09BC\
  \u09BE\u09B0\u09CD\u0995\u09C7\u09B0 \u09AE\u09BE\u09A7\u09CD\u09AF\u09AE\u09C7\
  \ \u099F\u09C7\u09B8\u09CD\u099F\u09BF\u0982 \u09B8\u09AE\u09B0\u09CD\u09A5\u09A8\
  \ \u0995\u09B0\u09C7, \u09AF\u09BE Xcode \u098F \u09B8\u09AE\u09A8\u09CD\u09AC\u09BF\
  \u09A4 \u09B9\u09AF\u09BC\u09C7\u099B\u09C7\u0964 \u0986\u09AA\u09A8\u09BF \u0986\
  \u09AA\u09A8\u09BE\u09B0 \u0995\u09CB\u09A1\u09C7\u09B0 \u09AA\u09CD\u09B0\u09A4\
  \u09BF\u099F\u09BF \u0985\u0982\u09B6 \u09AF\u09BE\u099A\u09BE\u0987 \u0995\u09B0\
  \u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u0987\u0989\u09A8\u09BF\u099F \u099F\u09C7\
  \u09B8\u09CD\u099F \u09B2\u09BF\u0996\u09A4\u09C7 \u09AA\u09BE\u09B0\u09C7\u09A8\
  , \u09AF\u09C7\u09AE\u09A8 \u09A6\u09C1\u099F\u09BF \u09B8\u0982\u0996\u09CD\u09AF\
  \u09BE \u09AF\u09CB\u0997 \u0995\u09B0\u09BE \u098F\u0995\u099F\u09BF \u09AB\u09BE\
  \u0982\u09B6\u09A8\u0964."
title: "\u099F\u09C7\u09B8\u09CD\u099F \u09B2\u09BF\u0996\u09BE"
weight: 36
---

## কীভাবে:
Swift এর XCTest ফ্রেমওয়ার্কের মাধ্যমে টেস্টিং সমর্থন করে, যা Xcode এ সমন্বিত হয়েছে। আপনি আপনার কোডের প্রতিটি অংশ যাচাই করার জন্য ইউনিট টেস্ট লিখতে পারেন, যেমন দুটি সংখ্যা যোগ করা একটি ফাংশন।

```swift
import XCTest
@testable import YourApp

class YourAppTests: XCTestCase {

    func testSum() {
        let result = Calculator().sum(a: 1, b: 2)
        XCTAssertEqual(result, 3, "যোগ ফাংশন প্রত্যাশিত মান ফেরত দেয়নি।")
    }
}
```

এই টেস্টটি চালানোর জন্য, আপনি সাধারণত Xcode এ Command-U চাপবেন। Xcode টেস্ট নেভিগেটরের আউটপুট আপনাকে জানাবে যে টেস্টটি পাস হয়েছে কিনা বা ব্যর্থ হয়েছে।

উদাহরণস্বরূপ, একটি সফল টেস্ট আউটপুট:
```
টেস্ট কেস '-[YourAppTests testSum]' সফল হয়েছে (0.005 সেকেন্ড)।
```

আরও উন্নত টেস্টিং সিনারিওর জন্য, আপনি তৃতীয়-পক্ষের লাইব্রেরিগুলি যেমন Quick/Nimble সম্পর্কে চিন্তা করতে পারেন, যা টেস্ট লেখার জন্য আরও ব্যক্তিগত বাক্যবিন্যাস প্রদান করে।

Quick/Nimble এর সাথে, আপনি একই টেস্টটি এরকম লিখতে পারেন:

```swift
// আপনার Swift প্যাকেজ ম্যানেজারে Quick এবং Nimble যোগ করুন অথবা তাদের ইনস্টল করতে CocoaPods/Carthage ব্যবহার করুন
import Quick
import Nimble
@testable import YourApp

class CalculatorSpec: QuickSpec {
    override func spec() {
        describe("Calculator") {
            context("when summing numbers") {
                it("should return the correct sum") {
                    let calculator = Calculator()
                    expect(calculator.sum(a: 1, b: 2)).to(equal(3))
                }
            }
        }
    }
}
```

এই টেস্টটি চালানো আপনার টেস্ট কনসোল বা CI/CD টুলের লগে সাফল্য বা ব্যর্থতার ইন্ডিকেশনের সাথে একই রকমের আউটপুট দেবে, টেস্ট এবং প্রত্যাশাগুলি বর্ণনার জন্য আরও পঠনযোগ্য ফর্ম্যাট সরবরাহ করে।
