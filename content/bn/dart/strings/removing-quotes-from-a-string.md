---
title:                "স্ট্রিং থেকে উদ্ধৃতি মুছে ফেলা"
date:                  2024-03-17T18:12:46.841341-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?
ডার্টে একটি স্ট্রিং থেকে উদ্ধৃতি চিহ্ন সরানো মানে হল স্ট্রিংয়ের শুরু এবং শেষ থেকে ডাবল (") বা সিঙ্গেল (') উদ্ধৃতি চিহ্নগুলি সরানো, যা ডাটা পরিষ্কার করা বা স্ট্রিংগুলি আরও প্রক্রিয়াজাত করার জন্য উপযোগী। প্রোগ্রামাররা এটি ডাটা ইনপুটগুলি স্বাভাবিক করতে, ডাটা সঞ্চয়ে একতানতা নিশ্চিত করতে বা এমন এপিআইগুলির সাথে ইন্টারফেসিং করার সময় করে থাকেন, যা উদ্ধৃতি বিন্যাসে ডাটা ফেরত দেয়।

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
