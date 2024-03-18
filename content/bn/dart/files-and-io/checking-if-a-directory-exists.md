---
title:                "ডিরেক্টরি আছে কিনা পরীক্ষা করা"
date:                  2024-03-17T17:45:41.632644-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?

Dart ব্যবহার করে একটি ডিরেক্টরির অস্তিত্ব পরীক্ষা করা মানে ফাইল সিস্টেমে নির্দিষ্ট পাথে একটি ডিরেক্টরির উপস্থিতি যাচাই করা, ফাইল পড়া বা লেখার মতো অপারেশন সঞ্চালনের আগে। প্রোগ্রামাররা এটি করে থাকেন এমন ত্রুটি এড়াতে যা ঘটে যখন তারা অস্তিত্বে নেই এমন ডিরেক্টরিগুলি অ্যাক্সেস বা পরিবর্তনের চেষ্টা করে।

## কিভাবে:

Dart ফাইল এবং ডিরেক্টরির সাথে কাজ করতে `dart:io` লাইব্রেরী ব্যবহার করে। একটি ডিরেক্টরির অস্তিত্ব পরীক্ষা করা একটি সাধারণ উপায় হলো:

```dart
import 'dart:io';

void main() {
  var directory = Directory('path/to/your/directory');

  if (directory.existsSync()) {
    print('Directory exists');
  } else {
    print('Directory does not exist');
  }
}
```
যদি ডিরেক্টরি অস্তিত্বে থাকে তবে নমুনা আউটপুট:
```
Directory exists
```

অথবা, যদি না থাকে:
```
Directory does not exist
```

আরও জটিল পরিস্থিতি, যেমন অ্যাসিঙ্ক্রোনাসলি চেক করা বা যদি ডিরেক্টরি না থাকে তবে তৈরী করা, সামলানোর জন্য নিম্নলিখিত পদ্ধতি ব্যবহার করা যেতে পারে:

```dart
import 'dart:io';

void main() async {
  var directory = Directory('path/to/your/directory');

  // অ্যাসিঙ্ক্রোনাসলি চেক করুন যে ডিরেক্টরির অস্তিত্ব আছে কিনা
  var exists = await directory.exists();
  if (exists) {
    print('Directory exists');
  } else {
    print('Directory does not exist, creating...');
    await directory.create(); // এটি ডিরেক্টরিটি তৈরী করে
    print('Directory created');
  }
}
```

যদি ডিরেক্টরি অস্তিত্বে না থাকে এবং তৈরি করা হয় তবে নমুনা আউটপুট:
```
Directory does not exist, creating...
Directory created
```

ফাইল এবং ডিরেক্টরিগুলি পরিচালনার জন্য Dart-এর অন্তর্নির্মিত ক্ষমতাগুলি সাধারণত পর্যাপ্ত থাকে, তাই এই কাজের জন্য তৃতীয় পক্ষের লাইব্রেরি সাধারণত প্রয়োজন হয় না। তবে, আরও জটিল ফাইল সিস্টেম অপারেশনের জন্য, `path` মত প্যাকেজগুলি (একটি প্ল্যাটফর্ম-নিরপেক্ষ উপায়ে পাথগুলি পরিবর্তন করার জন্য) `dart:io` লাইব্রেরীকে পরিপূরক করতে পারে কিন্তু প্রদর্শিত তুলনায় আরও উন্নত ডিরেক্টরি অস্তিত্ব চেকগুলি সরাসরি অফার করে না।
