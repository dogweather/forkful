---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:36:20.346957-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Dart \u098F\u09B0 \u09B8\u09CD\
  \u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09BE\u09B0\u09CD\u09A1 \u09B2\u09BE\u0987\
  \u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u09A4\u09C7 XML \u09B9\u09CD\u09AF\u09BE\u09A8\
  \u09CD\u09A1\u09B2\u09BF\u0982\u09AF\u09BC\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF\
  \ \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\u09A8 \u09B8\u09BE\u09AA\u09CB\u09B0\u09CD\
  \u099F \u09A8\u09C7\u0987, \u09AF\u09BE \u09A4\u09C3\u09A4\u09C0\u09AF\u09BC \u09AA\
  \u0995\u09CD\u09B7\u09C7\u09B0 \u09AA\u09CD\u09AF\u09BE\u0995\u09C7\u099C\u0997\u09C1\
  \u09B2\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0\u09C7\u09B0 \u09AA\u09CD\
  \u09B0\u09AF\u09BC\u09CB\u099C\u09A8 \u0995\u09B0\u09C7 \u09A4\u09CB\u09B2\u09C7\
  \u0964 \u098F\u0995\u099F\u09BF\u2026"
lastmod: '2024-03-17T18:47:43.745526-06:00'
model: gpt-4-0125-preview
summary: "Dart \u098F\u09B0 \u09B8\u09CD\u099F\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\
  \u09BE\u09B0\u09CD\u09A1 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF\u09A4\
  \u09C7 XML \u09B9\u09CD\u09AF\u09BE\u09A8\u09CD\u09A1\u09B2\u09BF\u0982\u09AF\u09BC\
  \u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09AC\u09BF\u09B2\u09CD\u099F-\u0987\u09A8\
  \ \u09B8\u09BE\u09AA\u09CB\u09B0\u09CD\u099F \u09A8\u09C7\u0987, \u09AF\u09BE \u09A4\
  \u09C3\u09A4\u09C0\u09AF\u09BC \u09AA\u0995\u09CD\u09B7\u09C7\u09B0 \u09AA\u09CD\
  \u09AF\u09BE\u0995\u09C7\u099C\u0997\u09C1\u09B2\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\
  \u09BE\u09B0\u09C7\u09B0 \u09AA\u09CD\u09B0\u09AF\u09BC\u09CB\u099C\u09A8 \u0995\
  \u09B0\u09C7 \u09A4\u09CB\u09B2\u09C7\u0964 \u098F\u0995\u099F\u09BF \u099C\u09A8\
  \u09AA\u09CD\u09B0\u09BF\u09AF\u09BC \u09AA\u09CD\u09AF\u09BE\u0995\u09C7\u099C\
  \ \u09B9\u09B2 `xml`\u0964 \u098F\u099F\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\
  \u09B0 \u0995\u09B0\u09A4\u09C7, \u09AA\u09CD\u09B0\u09A5\u09AE\u09C7 \u0986\u09AA\
  \u09A8\u09BE\u0995\u09C7 \u098F\u099F\u09BF \u0986\u09AA\u09A8\u09BE\u09B0 `pubspec.yaml`\
  \ \u098F \u09AF\u09CB\u0997 \u0995\u09B0\u09A4\u09C7 \u09B9\u09AC\u09C7."
title: "XML \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
weight: 40
---

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
