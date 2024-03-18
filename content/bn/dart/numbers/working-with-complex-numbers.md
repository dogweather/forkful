---
title:                "জটিল সংখ্যার সাথে কাজ করা"
date:                  2024-03-17T18:38:27.660930-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কী এবং কেন?

জটিল সংখ্যা, যা একটি বাস্তব এবং একটি কাল্পনিক অংশ (সাধারণত a + bi হিসেবে চিহ্নিত) নিয়ে গঠিত, মাত্রাশূন্য সংখ্যাগুলির ধারণাকে দ্বি-মাত্রিক স্থানে প্রসারিত করে। প্রোগ্রামাররা বৈদ্যুতিক প্রকৌশল, কোয়ান্টাম কম্পিউটিং এবং তরল গতিবিদ্যা যেমন ক্ষেত্রগুলিতে কাজ করে থাকেন, যেখানে কেবল বাস্তব সংখ্যার একক মাত্রায় প্রতিনিধিত্ব করা যায় না এমন ঘটনাগুলি মডেল করতে।

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
