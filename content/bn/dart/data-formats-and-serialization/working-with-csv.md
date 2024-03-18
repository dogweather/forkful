---
title:                "CSV এর সাথে কাজ করা"
date:                  2024-03-17T18:28:20.660982-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কী ও কেন?

CSV (কমা সেপারেটেড ভ্যালুস) ফাইলের সাথে কাজ করা মানে এমন টেক্সট ফাইল পারস করা এবং তৈরি করা যেখানে প্রতিটি লাইনে ভ্যালুগুলি কমা দ্বারা আলাদা হয়। প্রোগ্রামাররা এটা করে থাকে ভিন্ন অ্যাপ্লিকেশনের মধ্যে ডেটা আদান-প্রদান সম্ভব করতে অথবা ডেটা সংরক্ষণের জন্য লাইটওয়েট, মানুষের পড়া যায় এমন ফর্ম্যাট তৈরি করতে।

## কিভাবে:

Dart-এ CSV ফাইলগুলি সম্পর্কে কাজ করার জন্য, আপনি সাধারণত টেক্সটটি ম্যানুয়ালি প্রক্রিয়া করেন অথবা কাজটিকে সহজ করার জন্য থার্ড-পার্টি লাইব্রেরি ব্যবহার করেন। এখানে, আমরা উভয় পদ্ধতিকেই দেখব।

### ম্যানুয়ালি CSV পার্স করা

যদি আপনার প্রয়োজন সাধারণ হয়, আপনি একটি CSV স্ট্রিং ম্যানুয়ালি পার্স করতে পারেন। এটি Dart-এর মূল স্ট্রিং ম্যানিপুলেশান ফাংশনগুলি ব্যবহার করে অর্জন করা যায়:

```dart
void main() {
  // নমুনা CSV ডেটা
  String csvData = "Name,Age,Email\nJohn Doe,30,john@example.com\nJane Smith,25,jane@example.com";
  
  // CSV ডেটাকে লাইনগুলিতে বিভক্ত করা
  List<String> lines = csvData.split('\n');
  
  // প্রতিটি লাইন পারস করা
  List<Map<String, String>> data = [];
  List<String> headers = lines.first.split(',');
  
  for (var i = 1; i < lines.length; i++) {
    List<String> row = lines[i].split(',');
    Map<String, String> record = {};
    for (var j = 0; j < headers.length; j++) {
      record[headers[j]] = row[j];
    }
    data.add(record);
  }
  
  // পারস করা ডেটা আউটপুট করা
  print(data);
}

// নমুনা আউটপুট:
// [{Name: John Doe, Age: 30, Email: john@example.com}, {Name: Jane Smith, Age: 25, Email: jane@example.com}]
```

### থার্ড-পার্টি লাইব্রেরি ব্যবহার: `csv`

বেশি জটিল সিনারিওর জন্য অথবা আপনার কোডকে সহজ করতে, আপনি `csv` এর মতো জনপ্রিয় থার্ড-পার্টি লাইব্রেরি ব্যবহার করতে পারেন। প্রথমে, এটিকে `dependencies`-এর অধীনে আপনার `pubspec.yaml` ফাইলে `csv: ^5.0.0` (অথবা সর্বশেষ ভার্সন) যুক্ত করে প্রজেক্টে যুক্ত করুন। তারপর নিম্নের মত ব্যবহার করুন:

```dart
import 'package:csv/csv.dart';

void main() {
  String csvData = "Name,Age,Email\nJohn Doe,30,john@example.com\nJane Smith,25,jane@example.com";
  
  // CsvToListConverter ব্যবহার করে CSV ডেটা পার্স করা
  List<List<dynamic>> listData = const CsvToListConverter().convert(csvData);
  
  // প্রথম লিস্ট আইটেমে হেডার রয়েছে
  List<String> headers = listData.first.map((item) => item.toString()).toList();
  
  // আরো প্রসেসিং করার আগে হেডার রো সরিয়ে দেওয়া
  listData.removeAt(0);
  
  // আরও গোছালো ফর্ম্যাটের জন্য List<Map<String, dynamic>> এ রূপান্তর
  List<Map<String, dynamic>> mappedData = listData.map((list) {
    Map<String, dynamic> map = {};
    for (int i = 0; i < headers.length; i++) {
      map[headers[i]] = list[i];
    }
    return map;
  }).toList();
  
  // ম্যাপ করা ডেটা আউটপুট করা
  print(mappedData);
}

// নমুনা আউটপুট:
// [{Name: John Doe, Age: 30, Email: john@example.com}, {Name: Jane Smith, Age: 25, Email: jane@example.com}]
```

উভয় পদ্ধতি কিভাবে CSV ডেটা নিয়ে কাজ করতে হয় তা দেখিয়েছে: প্রথমটি ম্যানুয়ালি, শেখার উদ্দেশ্যে অথবা সহজ CSV গঠনের সাথে কাজ করার সময়; দ্বিতীয়টি, একটি শক্তিশালী লাইব্রেরি ব্যবহার করে যা পার্সিং সহজ করে এবং CSV ফর্ম্যাটের বিভিন্ন জটিলতা সামলাতে পারে।
