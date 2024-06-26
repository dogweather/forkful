---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:12:46.841341-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u09A1\u09BE\u09B0\u09CD\u099F\
  \ \u09A4\u09C3\u09A4\u09C0\u09AF\u09BC-\u09AA\u0995\u09CD\u09B7\u09C7\u09B0 \u09B2\
  \u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u0997\u09C1\u09B2\u09BF\u09B0 \u09AA\
  \u09CD\u09B0\u09AF\u09BC\u09CB\u099C\u09A8 \u099B\u09BE\u09A1\u09BC\u09BE \u09A8\
  \u09BF\u09B0\u09CD\u09AE\u09BF\u09A4 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982\
  \ \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF\u0997\u09C1\u09B2\u09BF \u09AC\u09CD\u09AF\
  \u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09B8\u09CD\u099F\u09CD\u09B0\u09BF\
  \u0982 \u09A5\u09C7\u0995\u09C7 \u0989\u09A6\u09CD\u09A7\u09C3\u09A4\u09BF \u09B8\
  \u09B0\u09BE\u09A8\u09CB\u09B0 \u09B8\u09B0\u09BE\u09B8\u09B0\u09BF \u0989\u09AA\
  \u09BE\u09AF\u09BC \u09AA\u09CD\u09B0\u09A6\u09BE\u09A8 \u0995\u09B0\u09C7\u0964\
  ."
lastmod: '2024-04-05T21:53:51.803342-06:00'
model: gpt-4-0125-preview
summary: "\u09A1\u09BE\u09B0\u09CD\u099F \u09A4\u09C3\u09A4\u09C0\u09AF\u09BC-\u09AA\
  \u0995\u09CD\u09B7\u09C7\u09B0 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\
  \u0997\u09C1\u09B2\u09BF\u09B0 \u09AA\u09CD\u09B0\u09AF\u09BC\u09CB\u099C\u09A8\
  \ \u099B\u09BE\u09A1\u09BC\u09BE \u09A8\u09BF\u09B0\u09CD\u09AE\u09BF\u09A4 \u09B8\
  \u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09AA\u09A6\u09CD\u09A7\u09A4\u09BF\u0997\u09C1\
  \u09B2\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 \u09B8\
  \u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u0989\u09A6\u09CD\
  \u09A7\u09C3\u09A4\u09BF \u09B8\u09B0\u09BE\u09A8\u09CB\u09B0 \u09B8\u09B0\u09BE\
  \u09B8\u09B0\u09BF \u0989\u09AA\u09BE\u09AF\u09BC \u09AA\u09CD\u09B0\u09A6\u09BE\
  \u09A8 \u0995\u09B0\u09C7\u0964."
title: "\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u0982 \u09A5\u09C7\u0995\u09C7 \u0989\
  \u09A6\u09CD\u09A7\u09C3\u09A4\u09BF \u09AE\u09C1\u099B\u09C7 \u09AB\u09C7\u09B2\
  \u09BE"
weight: 9
---

## কিভাবে:
ডার্ট তৃতীয়-পক্ষের লাইব্রেরিগুলির প্রয়োজন ছাড়া নির্মিত স্ট্রিং পদ্ধতিগুলি ব্যবহার করে স্ট্রিং থেকে উদ্ধৃতি সরানোর সরাসরি উপায় প্রদান করে।

### উদাহরণ 1: `replaceFirst` এবং `replaceAll` ব্যবহার করে
যদি আপনি এমন স্ট্রিংগুলির সাথে কাজ করছেন যা উদ্ধৃতি চিহ্ন দিয়ে শুরু এবং শেষ হয়, তাহলে আপনি `replaceFirst` এবং `replaceAll` পদ্ধতিগুলি ব্যবহার করে তা সরাতে পারেন।

```dart
String quotedString = '"Hello, World!"';
String singleQuotedString = '\'Dart Programming\'';

// ডাবল উদ্ধৃতি সরানো
String noDoubleQuotes = quotedString.replaceFirst('"', '').replaceAll('"', '');
print(noDoubleQuotes); // আউটপুট: Hello, World!

// সিঙ্গেল উদ্ধৃতি সরানো
String noSingleQuotes = singleQuotedString.replaceFirst('\'', '').replaceAll('\'', '');
print(noSingleQuotes); // আউটপুট: Dart Programming
```

### উদাহরণ 2: `substring` ব্যবহার করে
যদি আপনি নিশ্চিত হন যে উদ্ধৃতি চিহ্নগুলি স্ট্রিংয়ের খুব শুরু এবং শেষে আছে, তাহলে এই পদ্ধতিটি উপযোগী।

```dart
String quotedString = '"Flutter Development"';
// ভুল এড়াতে সরানোর আগে চেক করুন যে এটি উদ্ধৃতি চিহ্ন দিয়ে শুরু এবং শেষ হয়েছে কিনা
if (quotedString.startsWith('"') && quotedString.endsWith('"')) {
  quotedString = quotedString.substring(1, quotedString.length - 1);
}
print(quotedString); // আউটপুট: Flutter Development
```

### উদাহরণ 3: কাস্টম এক্সটেনশান পদ্ধতি
যদি আপনার প্রকল্পে বারবার উদ্ধৃতি সরানোর প্রয়োজন হয়, তাহলে আরও পুনরায় ব্যবহারযোগ্যতার জন্য `String`-এ একটি কাস্টম এক্সটেনশান তৈরি করা বিবেচনা করুন।

```dart
extension UnquoteString on String {
  String unquote() {
    var str = this;
    if (str.startsWith('"') && str.endsWith('"') || str.startsWith('\'') && str.endsWith('\'')) {
      str = str.substring(1, str.length - 1);
    }
    return str;
  }
}

void main() {
  String doubleQuoted = '"This is Dart"';
  String singleQuoted = '\'This is awesome\'';
  print(doubleQuoted.unquote()); // আউটপুট: This is Dart
  print(singleQuoted.unquote()); // আউটপুট: This is awesome
}
```

ডার্টে স্ট্রিং থেকে উদ্ধৃতি সরানোর এই পদ্ধতিগুলি আপনাকে আপনার ডাটা প্রসেসিং এবং প্রস্তুতি কাজে সাহায্য করবে।
