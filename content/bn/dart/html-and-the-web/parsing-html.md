---
title:                "HTML পার্স করা"
date:                  2024-03-17T18:04:02.024397-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?
প্রোগ্রামিংয়ে HTML পার্সিং মানে HTML ডকুমেন্ট থেকে ডেটা নিষ্কাষণ। প্রোগ্রামাররা তথ্য নিষ্কাষণ, পরীক্ষা, অথবা অটোমেশন উদ্দেশ্যে বা অফিসিয়াল API না থাকা সত্ত্বেও ওয়েব কন্টেন্টের সাথে মিথস্ক্রিয়া বা তথ্য নিষ্কাষণের জন্য এটি করে।

## কিভাবে:
Dart এর কোর লাইব্রেরিগুলিতে HTML পার্সিংয়ের জন্য কোন নির্মিত সহায়তা প্রদান করা হয় নি। তবে, `html` নামক থার্ড-পার্টি প্যাকেজ ব্যবহার করে আপনি HTML ডকুমেন্টগুলি পার্স এবং ম্যানিপুলেট করতে পারেন।

প্রথমে, `html` প্যাকেজটি আপনার `pubspec.yaml` ফাইলে যুক্ত করুন:

```yaml
dependencies:
  html: ^0.15.0
```

তারপর, প্যাকেজটি আপনার Dart ফাইলে আমদানি করুন:

```dart
import 'package:html/parser.dart' show parse;
import 'package:html/dom.dart';
```

HTML যুক্ত একটি স্ট্রিং পার্স করে এবং ডেটা নিষ্কাষণের একটি মৌলিক উদাহরণ এখানে দেওয়া হল:

```dart
void main() {
  var htmlDocument = """
  <html>
    <body>
      <h1>Hello, Dart!</h1>
      <p>This is a paragraph in a sample HTML</p>
    </body>
  </html>
  """;

  // HTML স্ট্রিং পার্স করুন
  Document document = parse(htmlDocument);

  // ডেটা নিষ্কাষণ
  String title = document.querySelector('h1')?.text ?? "কোনো শিরোনাম পাওয়া যায়নি";
  String paragraph = document.querySelector('p')?.text ?? "কোনো অনুচ্ছেদ পাওয়া যায়নি";

  print('শিরোনাম: $title');
  print('অনুচ্ছেদ: $paragraph');
}
```

আউটপুট:

```
শিরোনাম: Hello, Dart!
অনুচ্ছেদ: This is a paragraph in a sample HTML
```

বাস্তব বিশ্বের ওয়েব পেজগুলির সাথে ইন্টারঅ্যাক্ট করতে, আপনি `html` পার্সিংকে `http` প্যাকেজ সহ (ওয়েব কন্টেন্ট আনতে) HTTP অনুরোধ দিয়ে সম্মিলন করতে পারেন। এখানে একটি দ্রুত উদাহরণ দেওয়া হল:

প্রথমে, `http` প্যাকেজের সাথে `html` যুক্ত করুন:

```yaml
dependencies:
  html: ^0.15.0
  http: ^0.13.3
```

তারপর, ওয়েব থেকে একটি HTML পৃষ্ঠা আনুন এবং পার্স করুন:

```dart
import 'package:http/http.dart' as http;
import 'package:html/parser.dart' show parse;

void main() async {
  var url = 'https://example.com';
  
  // ওয়েবপেজটি আনুন
  var response = await http.get(Uri.parse(url));
  
  if (response.statusCode == 200) {
    var document = parse(response.body);

    // ধরুন পৃষ্ঠাটিতে আগ্রহের <h1> ট্যাগগুলি রয়েছে
    var headlines = document.querySelectorAll('h1').map((e) => e.text).toList();
    
    print('শিরোনামগুলি: $headlines');
  } else {
    print('অনুরোধ ব্যর্থ হয়েছে, স্থিতি: ${response.statusCode}.');
  }
}
```

নোট: উপরে দেখানো ওয়েব স্ক্রেপিং কৌশলটি দায়বদ্ধতার সাথে এবং ওয়েবসাইটের পরিষেবা শর্তাবলীর মান্যতা সাপেক্ষে ব্যবহার করা উচিত।
