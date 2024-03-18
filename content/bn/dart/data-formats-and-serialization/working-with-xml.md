---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:36:20.346957-06:00
description: "Dart \u098F XML \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C\
  \ \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 XML \u09A8\u09A5\u09BF\u0997\u09C1\
  \u09B2\u09BF \u09AA\u09BE\u09B0\u09CD\u09B8\u09BF\u0982, \u099C\u09BF\u099C\u09CD\
  \u099E\u09BE\u09B8\u09BE \u098F\u09AC\u0982 \u09B8\u0982\u09B6\u09CB\u09A7\u09A8\
  \ \u0995\u09B0\u09BE, \u09AF\u09BE \u0993\u09AF\u09BC\u09C7\u09AC \u09B8\u09C7\u09AC\
  \u09BE\u0997\u09C1\u09B2\u09BF, \u0995\u09A8\u09AB\u09BF\u0997\u09BE\u09B0\u09C7\
  \u09B6\u09A8 \u09AB\u09BE\u0987\u09B2, \u0985\u09A5\u09AC\u09BE \u09AA\u09C1\u09B0\
  \u09CB\u09A8\u09CB \u09B8\u09BF\u09B8\u09CD\u099F\u09C7\u09AE\u09C7\u09B0 \u09B8\
  \u09BE\u09A5\u09C7 \u09AE\u09BF\u09A5\u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\
  \u09BE\u2026"
lastmod: '2024-03-17T18:47:43.745526-06:00'
model: gpt-4-0125-preview
summary: "Dart \u098F XML \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C\
  \ \u0995\u09B0\u09BE \u09AE\u09BE\u09A8\u09C7 XML \u09A8\u09A5\u09BF\u0997\u09C1\
  \u09B2\u09BF \u09AA\u09BE\u09B0\u09CD\u09B8\u09BF\u0982, \u099C\u09BF\u099C\u09CD\
  \u099E\u09BE\u09B8\u09BE \u098F\u09AC\u0982 \u09B8\u0982\u09B6\u09CB\u09A7\u09A8\
  \ \u0995\u09B0\u09BE, \u09AF\u09BE \u0993\u09AF\u09BC\u09C7\u09AC \u09B8\u09C7\u09AC\
  \u09BE\u0997\u09C1\u09B2\u09BF, \u0995\u09A8\u09AB\u09BF\u0997\u09BE\u09B0\u09C7\
  \u09B6\u09A8 \u09AB\u09BE\u0987\u09B2, \u0985\u09A5\u09AC\u09BE \u09AA\u09C1\u09B0\
  \u09CB\u09A8\u09CB \u09B8\u09BF\u09B8\u09CD\u099F\u09C7\u09AE\u09C7\u09B0 \u09B8\
  \u09BE\u09A5\u09C7 \u09AE\u09BF\u09A5\u09B8\u09CD\u0995\u09CD\u09B0\u09BF\u09AF\u09BC\
  \u09BE\u2026"
title: "XML \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?

Dart এ XML এর সাথে কাজ করা মানে XML নথিগুলি পার্সিং, জিজ্ঞাসা এবং সংশোধন করা, যা ওয়েব সেবাগুলি, কনফিগারেশন ফাইল, অথবা পুরোনো সিস্টেমের সাথে মিথস্ক্রিয়া করা অ্যাপ্লিকেশনগুলির জন্য অপরিহার্য একটি প্রক্রিয়া। প্রোগ্রামাররা এটি ডেটা বিনিময়, কনফিগারেশন, অথবা এমনকি রিমোট প্রসিজার কলের জন্য করে থাকেন একটি structured, hierarchical format এ যা মানুষের পাঠ্য এবং মেশিনের পার্সযোগ্য উভয়ের জন্যই।

## কিভাবে:

Dart এর স্ট্যান্ডার্ড লাইব্রেরিতে XML হ্যান্ডলিংয়ের জন্য বিল্ট-ইন সাপোর্ট নেই, যা তৃতীয় পক্ষের প্যাকেজগুলি ব্যবহারের প্রয়োজন করে তোলে। একটি জনপ্রিয় প্যাকেজ হল `xml`। এটি ব্যবহার করতে, প্রথমে আপনাকে এটি আপনার `pubspec.yaml` এ যোগ করতে হবে:

```yaml
dependencies:
  xml: ^5.0.0 // সর্বশেষ ভার্সন ব্যবহার করুন
```

তারপর, আপনার Dart ফাইলে প্যাকেজটি আমদানি করুন:

```dart
import 'package:xml/xml.dart' as xml;
```

**XML পার্সিং:**

ধরুন আপনার কাছে এমন একটি XML স্ট্রিং আছে:

```xml
<String name="greeting">Hello, world!</String>
```

আপনি নিম্নলিখিতভাবে XML পার্স এবং পড়তে পারেন:

```dart
void parseXml(String xmlString) {
    final document = xml.XmlDocument.parse(xmlString);
    final String content = document.findElements('String').single.getAttribute('name');
    print(content); // আউটপুট: greeting
}

void main() {
  final xmlString = '<String name="greeting">Hello, world!</String>';
  parseXml(xmlString);
}
```

**XML নথি তৈরি:**

`xml` প্যাকেজের সাথে একটি নতুন XML নথি তৈরি করা সরল:

```dart
void createXml() {
  final builder = xml.XmlBuilder();
  builder.processing('xml', 'version="1.0"');
  builder.element('greeting', nest: () {
    builder.attribute('name', 'hello');
    builder.text('Hello, world!');
  });
  final xmlDocument = builder.buildDocument();
  print(xmlDocument.toXmlString(pretty: true));
}

void main() {
  createXml();
}
```

**আউটপুট**:

```xml
<?xml version="1.0"?>
<greeting name="hello">Hello, world!</greeting>
```

**XML জিজ্ঞাসাবাদ এবং সংশোধন:**

উপাদানগুলি খুঁজে পেতে অথবা সংশোধন করতে, আপনি XPath এর মতো পদ্ধতি ব্যবহার করতে পারেন:

```dart
void modifyXml(String xmlString) {
    var document = xml.XmlDocument.parse(xmlString);
    var greeting = document.findAllElements('greeting').first;
    
    // 'name' অ্যাট্রিবিউট সংশোধন করা
    greeting.setAttribute('name', 'greeting_modified');
    
    // একটি নতুন চাইল্ড উপাদান যোগ করা
    greeting.children.add(xml.XmlElement(xml.XmlName('message'), [], [xml.XmlText('Goodbye!')]));
    
    print(document.toXmlString(pretty: true));
}

void main() {
  final xmlString = '<greeting name="hello">Hello, world!</greeting>';
  modifyXml(xmlString);
}
```

**আউটপুট**:

```xml
<greeting name="greeting_modified">
  Hello, world!
  <message>Goodbye!</message>
</greeting>
```

এই উদাহরণগুলি Dart এ XML এর সাথে কাজ করার জন্য মৌলিক অপারেশনগুলি দেখায়। `xml` প্যাকেজের সাথে, আপনি আপনার অ্যাপ্লিকেশনের প্রয়োজন মেটাতে XML নথিগুলি পার্স, তৈরি, এবং ম্যানিপুলেট করতে পারবেন।
