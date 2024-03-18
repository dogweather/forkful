---
title:                "টেস্ট লিখা"
date:                  2024-03-17T18:40:46.115938-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কী এবং কেন?

আরডুইনো পরিবেশে টেস্ট লেখার প্রক্রিয়াটি আরডুইনো ডিভাইসগুলিতে আপনার কোডের কার্যকারিতা যাচাই করে এমন স্বয়ংক্রিয় টেস্ট তৈরি করাকে নির্দেশ করে। প্রোগ্রামাররা এটি নিশ্চিত করতে করে থাকেন যে তাদের কোড প্রত্যাশামাফিক কাজ করে, বাগ কমায়, এবং বিশেষ করে এমবেডেড সিস্টেমে, যেখানে ডিবাগিং আরো জটিল হতে পারে, তাদের প্রকল্পের মান উন্নত করে।

## কিভাবে:

আরডুইনো অন্য কিছু প্রোগ্রামিং পরিবেশের মতো বিল্ট-ইন টেস্টিং ফ্রেমওয়ার্ক নেই। তবে, `AUnit` এর মতো থার্ড-পার্টি লাইব্রেরি ব্যবহার করে আরডুইনো কোডের ইউনিট টেস্টিং করতে পারেন। AUnit আরডুইনোর বিল্ট-ইন লাইব্রেরি `ArduinoUnit`, এবং গুগলের টেস্টিং ফ্রেমওয়ার্ক `Google Test` দ্বারা অনুপ্রাণিত।

### AUnit এর সাথে উদাহরণ:

প্রথমে, আরডুইনো IDE-এ লাইব্রেরি ম্যানেজার মারফত AUnit ইন্সটল করুন: Sketch > Include Library > Manage Libraries... > AUnit সার্চ করে ইন্সটল করুন।

তারপর, আপনি নিম্নলিখিতভাবে টেস্ট লিখতে পারেন:

```cpp
#include <AUnit.h>

test(ledPinHigh) {
  const int ledPin = 13;
  pinMode(ledPin, OUTPUT);
  digitalWrite(ledPin, HIGH);
  assertTrue(digitalRead(ledPin));
}

test(ledPinLow) {
  const int ledPin = 13;
  pinMode(ledPin, OUTPUT);
  digitalWrite(ledPin, LOW);
  assertFalse(digitalRead(ledPin));
}

void setup() {
  Serial.begin(9600);
  aunit::TestRunner::run();
}

void loop() {
  // খালি
}
```
এই টেস্টটি আপনার আরডুইনো বোর্ডে আপলোড করার পর, টেস্ট ফলাফল দেখতে সিরিয়াল মনিটর খুলুন। আপনি প্রতিটি টেস্ট পাস হয়েছে কিনা বা ব্যর্থ হয়েছে কিনা তা নির্দেশ করে এমন আউটপুট দেখতে পাবেন:

```
TestRunner started on 2 test(s).
Test ledPinHigh passed.
Test ledPinLow passed.
TestRunner duration: 0.002 seconds.
TestRunner summary: 2 passed, 0 failed, 0 skipped, 0 timed out, out of 2 test(s).
```

এই সহজ উদাহরণটি বিভিন্ন অবস্থার মধ্যে আপনার আরডুইনো প্রত্যাশামাফিক আচরণ করছে কিনা তা নিশ্চিত করার জন্য AUnit ব্যবহারে টেস্টিং এর অবস্থানের পরীক্ষা দেখায়। AUnit এর সাথে, আপনি আরও জটিল টেস্ট, টেস্ট স্যুট, এবং আরও উন্নত সিনারিওর জন্য টেস্ট টাইমআউট এবং সেটআপ/টিয়ারডাউন প্রক্রিয়ার মতো বৈশিষ্ট্য উপভোগ করতে পারেন।
