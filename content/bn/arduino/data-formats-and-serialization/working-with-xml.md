---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:31:27.215067-06:00
description: "Arduino-\u09A4\u09C7 XML \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\
  \u09BE\u099C \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u0993\u09AF\u09BC\u09C7\
  \u09AC API \u0985\u09A5\u09AC\u09BE \u0995\u09A8\u09AB\u09BF\u0997\u09BE\u09B0\u09C7\
  \u09B6\u09A8 \u09AB\u09BE\u0987\u09B2 \u09A5\u09C7\u0995\u09C7 \u0986\u09B8\u09BE\
  \ XML \u09A1\u09C7\u099F\u09BE \u09AA\u09BE\u09B0\u09CD\u09B8 \u098F\u09AC\u0982\
  \ \u09AE\u09CD\u09AF\u09BE\u09A8\u09BF\u09AA\u09C1\u09B2\u09C7\u099F \u0995\u09B0\
  \u09BE\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\
  \u09BE \u098F\u099F\u09BF \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09C7\u0964 \u09AF\
  \u09BE\u09B0\u09BE XML\u2026"
lastmod: '2024-03-17T18:47:44.346351-06:00'
model: gpt-4-0125-preview
summary: "Arduino-\u09A4\u09C7 XML \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\
  \u099C \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 \u0993\u09AF\u09BC\u09C7\u09AC\
  \ API \u0985\u09A5\u09AC\u09BE \u0995\u09A8\u09AB\u09BF\u0997\u09BE\u09B0\u09C7\u09B6\
  \u09A8 \u09AB\u09BE\u0987\u09B2 \u09A5\u09C7\u0995\u09C7 \u0986\u09B8\u09BE XML\
  \ \u09A1\u09C7\u099F\u09BE \u09AA\u09BE\u09B0\u09CD\u09B8 \u098F\u09AC\u0982 \u09AE\
  \u09CD\u09AF\u09BE\u09A8\u09BF\u09AA\u09C1\u09B2\u09C7\u099F \u0995\u09B0\u09BE\u0964\
  \ \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u098F\u099F\u09BF \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09C7\u0964 \u09AF\u09BE\
  \u09B0\u09BE XML \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09A1\
  \u09C7\u099F\u09BE \u098F\u0995\u09CD\u09B8\u099A\u09C7\u099E\u09CD\u099C \u0985\
  \u09A5\u09AC\u09BE \u09B8\u0982\u09B0\u0995\u09CD\u09B7\u09A3\u09C7\u09B0 \u099C\
  \u09A8\u09CD\u09AF \u09B8\u0982\u0997\u09A0\u09BF\u09A4, \u09AE\u09BE\u09A8\u09AC\
  -\u09AA\u09BE\u09A0\u09AF\u09CB\u0997\u09CD\u09AF \u09AB\u09B0\u09AE\u09CD\u09AF\
  \u09BE\u099F\u09C7 \u09A1\u09C7\u099F\u09BE \u09B8\u0982\u09B0\u0995\u09CD\u09B7\
  \u09A3 \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u09B8\u09C7\u09AC\u09BE\
  \ \u09B8\u09AE\u09C2\u09B9\u09C7\u09B0 \u09B8\u09BE\u09A5\u09C7 \u098F\u0995\u09C0\
  \u09AD\u09C2\u09A4 \u0995\u09B0\u09A4\u09C7 \u099A\u09BE\u09DF\u0964."
title: "XML \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
weight: 40
---

## কি এবং কেন?
Arduino-তে XML এর সাথে কাজ করা মানে ওয়েব API অথবা কনফিগারেশন ফাইল থেকে আসা XML ডেটা পার্স এবং ম্যানিপুলেট করা। প্রোগ্রামাররা এটি করে থাকে। যারা XML ব্যবহার করে ডেটা এক্সচেঞ্জ অথবা সংরক্ষণের জন্য সংগঠিত, মানব-পাঠযোগ্য ফরম্যাটে ডেটা সংরক্ষণ করার জন্য সেবা সমূহের সাথে একীভূত করতে চায়।

## কিভাবে:
আমরা XML তৈরির জন্য `XMLWriter` লাইব্রেরি এবং এটি পার্স করার জন্য `tinyxml2` লাইব্রেরি ব্যবহার করব। প্রথমে Arduino IDE-এর Library Manager এর মাধ্যমে লাইব্রেরিগুলি ইনস্টল করুন।

XML ডকুমেন্ট তৈরি:

```Arduino
#include <XMLWriter.h>

void setup() {
  Serial.begin(9600);
  
  XMLWriter xml(&Serial); // আউটপুটের জন্য Serial ব্যবহার করা
  
  xml.header();
  xml.tag("greeting").tag("text").text("Hello, world!").close().close();
  xml.flush();
}

void loop() {
}
```

XML স্ট্রিং ডিকোড:

```Arduino
#include <tinyxml2.h>

tinyxml2::XMLDocument doc;
doc.Parse("<greeting><text>Hello, world!</text></greeting>");

tinyxml2::XMLElement* text = doc.FirstChildElement("greeting")->FirstChildElement("text");
if (text != nullptr) {
  Serial.println(text->GetText());
}
```

নমুনা আউটপুট:

```
<greeting>
  <text>Hello, world!</text>
</greeting>
```

## গভীর ডুব
XML, বা Extensible Markup Language, এমন একটি মার্কআপ ভাষা যা এমন একটি সেট অফ রুল ডিফাইন করে যা ডকুমেন্টগুলি মানুষ এবং মেশিন উভয়ের কাছেই পঠনযোগ্য ফরম্যাটে এনকোড করার জন্য। এটি ৯০-এর দশকের শেষ নাগাদ আবির্ভূত হয়েছিল এবং বিভিন্ন ক্ষেত্রে, বিশেষ করে যেখানে প্ল্যাটফর্ম-স্বতন্ত্র ডেটা এক্সচেঞ্জ প্রয়োজন হয়, ব্যাপক ভাবে ব্যবহার করা হয়। Arduino-এর সীমিত মেমোরি সাধারণভাবে PC তে XML এর সাথে কাজ করা থেকে আরো বেশি চ্যালেঞ্জিং করে তোলে। তাই হালকা লাইব্রেরি গুরুত্বপূর্ণ। যদিও JSON এর সরল সিনট্যাক্স এবং ছোট ফুটপ্রিন্টের কারণে ডেটা এক্সচেঞ্জের জন্য জনপ্রিয়তা লাভ করেছে, XML, বিশেষ করে যখন পুরানো সিস্টেম বা স্কিমাগুলির মাধ্যমে ডকুমেন্ট ভ্যালিডেশনের প্রয়োজন হয়, তখন ব্যপকভাবে ব্যবহার করা হয়। Arduino XML বাস্তবায়নের মূল উপাদান হল স্ট্রিম পার্সিং, যা ডকুমেন্টের সেগমেন্টগুলো পড়ে মেমোরি ব্যবহারকে কম রাখে।

## আরও দেখুন
- [TinyXML-2 লাইব্রেরি ডকুমেন্টেশন](https://leethomason.github.io/tinyxml2/)
- JSON ডেটা নিয়ে কাজ করার সময় বিকল্প হিসাবে [Arduino JSON লাইব্রেরি](https://arduinojson.org/)
- সাধারণ XML শেখার জন্য [W3Schools XML টিউটোরিয়াল](https://www.w3schools.com/xml/)
- অফিসিয়াল XML স্ট্যান্ডার্ড এবং রেকমেন্ডেশনের জন্য [W3C XML স্পেসিফিকেশন](https://www.w3.org/XML/)
