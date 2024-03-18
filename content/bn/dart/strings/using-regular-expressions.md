---
title:                "রেগুলার এক্সপ্রেশন ব্যবহার করা"
date:                  2024-03-17T18:26:13.319604-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?
Dart-এ নিয়মিত ব্যক্তিক (regex) সক্ষমতা প্রদান করে যা স্ট্রিং অনুসন্ধান এবং পরিবর্তনের উপর শক্তিশালী উপায় নিশ্চিত করে থাকে, এটি প্রোগ্রামারদের জটিল টেক্সট প্রক্রিয়াকরণ কাজ দক্ষতার সাথে সম্পাদন করতে সাহায্য করে। Regex বুঝতে পারলে, ডেভেলপাররা টেক্সট যাচাইকরণ, অনুসন্ধানের ধরণ এবং টেক্সট পরিবর্তন দ্রুত সম্পাদন করতে পারে, যা আধুনিক অ্যাপ্লিকেশনে ফর্ম প্রক্রিয়াকরণ, ডেটা পার্সিং, এবং সাধারণ স্ট্রিং ম্যানিপুলেশন এর জন্য অপরিহার্য।

## কিভাবে:
Dart নিয়মিত ব্যক্তিকের জন্য `RegExp` ক্লাস ব্যবহার করে। এখানে একটি স্ট্রিংয়ে একটি সহজ প্যাটার্ন ম্যাচ করার একটি মৌলিক উদাহরণ দেওয়া হলঃ

```dart
void main() {
  var pattern = RegExp(r'\bDart\b');
  var text = 'Learning Dart programming is exciting.';

  if (pattern.hasMatch(text)) {
    print('মিল পাওয়া গেছে!');
  } else {
    print('কোন মিল পাওয়া যায়নি।');
  }
  // আউটপুট: মিল পাওয়া গেছে!
}
```

একটি স্ট্রিং থেকে ম্যাচগুলি এক্সট্র্যাক্ট করতে, আপনি `allMatches` মেথড ব্যবহার করতে পারেন। এই মেথডটি ম্যাচের একটি iterable ফেরত দেয়:

```dart
void main() {
  var pattern = RegExp(r'\b\w+\b');
  var text = 'Dart is awesome!';

  var matches = pattern.allMatches(text);
  for (final match in matches) {
    print(match.group(0)); // এটি মেলে যাওয়া সাবস্ট্রিং গুলি প্রিন্ট করে।
  }
  // আউটপুট:
  // Dart
  // is
  // awesome
}
```

টেক্সট পরিবর্তন করা `replaceFirst` বা `replaceAll` মেথড ব্যবহার করে সম্পন্ন করা যেতে পারেঃ

```dart
void main() {
  var pattern = RegExp(r'\bDart\b');
  var text = 'Dart is not just a dart.';
  
  // প্রথম ঘটনা পরিবর্তন
  var modifiedText = text.replaceFirst(pattern, 'Flutter');
  print(modifiedText); 
  // আউটপুট: Flutter is not just a dart.

  // সব ঘটনা পরিবর্তন
  modifiedText = text.replaceAll(pattern, 'Flutter');
  print(modifiedText);
  // আউটপুট: Flutter is not just a flutter.
}
```

একটি স্ট্রিংয়ের একটি regex প্যাটার্ন দ্বারা বিভাজন সহজ `split` মেথড ব্যবহার করে করা যায়ঃ

```dart
void main() {
  var pattern = RegExp(r'\s+'); // যেকোনো স্পেস অক্ষরের সাথে ম্যাচ করে
  var text = 'Dart is fun';

  var parts = text.split(pattern);
  print(parts); 
  // আউটপুট: [Dart, is, fun]
}
```

Dart-এর `RegExp` দ্বারা সরাসরি সমর্থিত না হওয়া জটিল পার্সিং বা যাচাইকরণের জন্য, আপনি তৃতীয় পক্ষের লাইব্রেরিগুলি বিবেচনা করতে পারেন, তবে Dart-এর স্ট্যান্ডার্ড লাইব্রেরি প্রায়ই সাধারণ regex কাজের জন্য যথেষ্ট, এটি নিয়মিত ব্যক্তিক সামলানোর ক্ষেত্রে এর উপযোগিতা এবং বহুমুখিতার উপর জোর দেয়।
