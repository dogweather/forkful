---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:38:27.660930-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Dart \u09A8\u09BF\u099C\u09C7\u0987\
  \ \u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u09B0 \u099C\u09A8\
  \u09CD\u09AF \u0995\u09CB\u09A8 \u0985\u09A8\u09CD\u09A4\u09B0\u09CD\u09A8\u09BF\
  \u09B0\u09CD\u09AE\u09BF\u09A4 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\
  \ \u0985\u09A8\u09CD\u09A4\u09B0\u09CD\u09AD\u09C1\u0995\u09CD\u09A4 \u0995\u09B0\
  \u09C7 \u09A8\u09BE, \u09AF\u09BE \u098F\u0995\u099F\u09BF \u0995\u09BE\u09B8\u09CD\
  \u099F\u09AE \u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE \u0995\
  \u09CD\u09B2\u09BE\u09B8\u09C7\u09B0 \u09AC\u09BE\u09B8\u09CD\u09A4\u09AC\u09BE\u09AF\
  \u09BC\u09A8 \u0985\u09A5\u09AC\u09BE \u09A4\u09C3\u09A4\u09C0\u09AF\u09BC-\u09AA\
  \u0995\u09CD\u09B7\u09C7\u09B0\u2026"
lastmod: '2024-04-05T21:53:51.815085-06:00'
model: gpt-4-0125-preview
summary: "Dart \u09A8\u09BF\u099C\u09C7\u0987 \u099C\u099F\u09BF\u09B2 \u09B8\u0982\
  \u0996\u09CD\u09AF\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u0995\u09CB\u09A8 \u0985\
  \u09A8\u09CD\u09A4\u09B0\u09CD\u09A8\u09BF\u09B0\u09CD\u09AE\u09BF\u09A4 \u09B2\u09BE\
  \u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u0985\u09A8\u09CD\u09A4\u09B0\u09CD\u09AD\
  \u09C1\u0995\u09CD\u09A4 \u0995\u09B0\u09C7 \u09A8\u09BE, \u09AF\u09BE \u098F\u0995\
  \u099F\u09BF \u0995\u09BE\u09B8\u09CD\u099F\u09AE \u099C\u099F\u09BF\u09B2 \u09B8\
  \u0982\u0996\u09CD\u09AF\u09BE \u0995\u09CD\u09B2\u09BE\u09B8\u09C7\u09B0 \u09AC\
  \u09BE\u09B8\u09CD\u09A4\u09AC\u09BE\u09AF\u09BC\u09A8 \u0985\u09A5\u09AC\u09BE\
  \ \u09A4\u09C3\u09A4\u09C0\u09AF\u09BC-\u09AA\u0995\u09CD\u09B7\u09C7\u09B0 \u09B2\
  \u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u09AA\u09CD\u09B0\u09AF\u09BC\u09CB\u099C\u09A8 \u0995\u09B0\u09C7\u0964\
  \ \u09AC\u09C8\u099C\u09CD\u099E\u09BE\u09A8\u09BF\u0995 \u0997\u09A3\u09A8\u09BE\
  \u09B0 \u0995\u09BE\u099C\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u098F\u0995\u099F\
  \u09BF \u099C\u09A8\u09AA\u09CD\u09B0\u09BF\u09AF\u09BC \u09AA\u099B\u09A8\u09CD\
  \u09A6, \u09AF\u09BE \u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE\
  \u09B0 \u099C\u09A8\u09CD\u09AF \u09B8\u09AE\u09B0\u09CD\u09A5\u09A8 \u0985\u09A8\
  \u09CD\u09A4\u09B0\u09CD\u09AD\u09C1\u0995\u09CD\u09A4 \u0995\u09B0\u09C7, \u09A4\
  \u09BE \u09B9\u09B2 `package:scidart`\u0964."
title: "\u099C\u099F\u09BF\u09B2 \u09B8\u0982\u0996\u09CD\u09AF\u09BE\u09B0 \u09B8\
  \u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
weight: 14
---

## কিভাবে:
Dart নিজেই জটিল সংখ্যার জন্য কোন অন্তর্নির্মিত লাইব্রেরি অন্তর্ভুক্ত করে না, যা একটি কাস্টম জটিল সংখ্যা ক্লাসের বাস্তবায়ন অথবা তৃতীয়-পক্ষের লাইব্রেরি ব্যবহার প্রয়োজন করে। বৈজ্ঞানিক গণনার কাজের জন্য একটি জনপ্রিয় পছন্দ, যা জটিল সংখ্যার জন্য সমর্থন অন্তর্ভুক্ত করে, তা হল `package:scidart`।

### একটি মৌলিক জটিল সংখ্যা ক্লাস বাস্তবায়ন
সাধারণ অপারেশনগুলির জন্য, আপনি সহজেই নিজের জটিল সংখ্যা ক্লাস সংজ্ঞায়িত করতে পারেন:

```dart
class Complex {
  final double real;
  final double imaginary;

  Complex(this.real, this.imaginary);

  // দুটি জটিল সংখ্যার যোগফল
  Complex operator +(Complex other) {
    return Complex(real + other.real, imaginary + other.imaginary);
  }

  // সহজ ডিবাগিংয়ের জন্য স্ট্রিং প্রতিনিধিত্ব
  @override
  String toString() => '${real} + ${imaginary}i';
}

void main() {
  var number1 = Complex(3, 4);
  var number2 = Complex(1, 2);

  var sum = number1 + number2;
  print(sum);  // 4.0 + 6.0i
}
```

### সাইডার্ট ব্যবহার করে উন্নত অপারেশনগুলি
আরও জটিল অপারেশনগুলি অথবা যখন পারফরমেন্স সমালোচনামূলক হয়, `package:scidart` জটিল সংখ্যার মধ্যে অন্যান্য বৈজ্ঞানিক গণনার কার্যকারিতার জন্য বিস্তৃত সমর্থন প্রদান করে। প্রথমত, SciDart যোগ করুন আপনার pubspec.yaml তে:

```yaml
dependencies:
  scidart: ^0.0.1-dev.9
```

এখানে SciDart ব্যবহার করে জটিল সংখ্যার সাথে মৌলিক অপারেশনগুলি সঞ্চালনের উপায় দেখানো হল:

```dart
import 'package:scidart/numdart.dart';

void main() {
  // জটিল সংখ্যা তৈরি করা
  var complexNum1 = Complex(real: 5, imaginary: 3);
  var complexNum2 = Complex(real: 2, imaginary: 7);

  // যোগ
  var sum = complexAdd(complexNum1, complexNum2);
  
  // গুণন
  var product = complexMultiply(complexNum1, complexNum2);

  print('Sum: ${sum.toString()}');  // সম্মান: Complex(real: 7.0, imaginary: 10.0)
  print('Product: ${product.toString()}');  // পণ্য: Complex(real: -11.0, imaginary: 41.0)
}
```

এই উদাহরণগুলি Dart এ জটিল সংখ্যার মৌলিক ম্যানিপুলেশন এবং ব্যবহারের প্রদর্শন করে, কাস্টম বাস্তবায়ন এবং SciDart লাইব্রেরির মাধ্যমে, যা বৈজ্ঞানিক গণনার কাজের জন্য Dart এর নমনীয়তা এবং শক্তির প্রদর্শন করে।
