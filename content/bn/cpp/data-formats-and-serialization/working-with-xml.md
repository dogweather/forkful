---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:31:37.680242-06:00
description: "XML-\u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\
  \u09BE \u09AE\u09BE\u09A8\u09C7 XML (eXtensible Markup Language) \u09A1\u09C7\u099F\
  \u09BE \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE, \u09A4\u09C8\u09B0\u09BF\
  \ \u0995\u09B0\u09BE, \u098F\u09AC\u0982 \u09AE\u09CD\u09AF\u09BE\u09A8\u09BF\u09AA\
  \u09C1\u09B2\u09C7\u099F \u0995\u09B0\u09BE\u0964 \u098F\u09B0 \u09AA\u09CD\u09B2\
  \u09CD\u09AF\u09BE\u099F\u09AB\u09B0\u09CD\u09AE-\u09A8\u09BF\u09B0\u09AA\u09C7\u0995\
  \u09CD\u09B7 \u09AA\u09CD\u09B0\u0995\u09C3\u09A4\u09BF\u09B0 \u0995\u09BE\u09B0\
  \u09A3\u09C7\u2026"
lastmod: '2024-03-17T18:47:44.389679-06:00'
model: gpt-4-0125-preview
summary: "XML-\u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\
  \u09BE \u09AE\u09BE\u09A8\u09C7 XML (eXtensible Markup Language) \u09A1\u09C7\u099F\
  \u09BE \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE, \u09A4\u09C8\u09B0\u09BF\
  \ \u0995\u09B0\u09BE, \u098F\u09AC\u0982 \u09AE\u09CD\u09AF\u09BE\u09A8\u09BF\u09AA\
  \u09C1\u09B2\u09C7\u099F \u0995\u09B0\u09BE\u0964 \u098F\u09B0 \u09AA\u09CD\u09B2\
  \u09CD\u09AF\u09BE\u099F\u09AB\u09B0\u09CD\u09AE-\u09A8\u09BF\u09B0\u09AA\u09C7\u0995\
  \u09CD\u09B7 \u09AA\u09CD\u09B0\u0995\u09C3\u09A4\u09BF\u09B0 \u0995\u09BE\u09B0\
  \u09A3\u09C7 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\
  \u09BE \u09B8\u09CD\u099F\u09CD\u09B0\u09BE\u0995\u099A\u09BE\u09B0\u09CD\u09A1\
  \ \u09A1\u09C7\u099F\u09BE \u0987\u09A8\u09CD\u099F\u09BE\u09B0\u099A\u09C7\u099E\
  \u09CD\u099C, \u0995\u09A8\u09AB\u09BF\u0997\u09BE\u09B0\u09C7\u09B6\u09A8, \u098F\
  \u09AC\u0982 \u0986\u09B0\u0993 \u0985\u09A8\u09C7\u0995 \u0995\u09BF\u099B\u09C1\
  \ \u09B8\u09BE\u09AE\u09B2\u09BE\u09A8\u09CB\u09B0 \u099C\u09A8\u09CD\u09AF XML\
  \ \u09AE\u09CD\u09AF\u09BE\u09A8\u09C7\u099C \u0995\u09B0\u09C7\u0964."
title: "XML \u09A8\u09BF\u09AF\u09BC\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
weight: 40
---

## কিভাবে:
TinyXML-2 লাইব্রেরি ব্যবহার করে XML পার্স করার একটি সহজ উপায় এখানে:

```C++
#include <tinyxml2.h>
#include <iostream>

int main() {
    tinyxml2::XMLDocument doc;
    doc.Parse("<root><message>হ্যালো, ওয়ার্ল্ড!</message></root>");
    const char* content = doc.FirstChildElement("root")->FirstChildElement("message")->GetText();
    std::cout << content << std::endl;
    return 0;
}
```

নমুনা আউটপুট:

```
হ্যালো, ওয়ার্ল্ড!
```

এবং এইভাবে আপনি একটি XML ফাইল তৈরি করতে পারেন:

```C++
#include <tinyxml2.h>
#include <iostream>

int main() {
    tinyxml2::XMLDocument doc;
    auto* declaration = doc.NewDeclaration();
    doc.InsertFirstChild(declaration);
    auto* root = doc.NewElement("root");
    doc.InsertEndChild(root);
    auto* message = doc.NewElement("message");
    message->SetText("হ্যালো, ওয়ার্ল্ড!");
    root->InsertEndChild(message);
    doc.SaveFile("output.xml");
    return 0;
}
```

এর ফলে `output.xml` নামে একটি XML ফাইল তৈরি হয় যার কন্টেন্ট হল:

```xml
<?xml version="1.0"?>
<root>
    <message>হ্যালো, ওয়ার্ল্ড!</message>
</root>
```

## গভীর ডুব
৯০ এর দশকের শেষের দিকে ওয়েব সার্ভিসেস এবং ডেটা স্টোরেজে XML প্রধান একটি ভূমিকা পালন করে। যদিও কনফিগ এবং ইন্টারঅপের জন্য JSON এবং YAML এখন আরও প্রচলিত, XML এখনও অনেক এন্টারপ্রাইজ সিস্টেমে বিশাল ভূমিকা পালন করে। C++-এ XML পার্স করা ম্যানুয়াল ডম/স্যাক্স পার্সিং দিয়ে পুরাতন ধাঁচের মনে হতে পারে। ধন্যবাদবাদ্য, TinyXML-2 এর মতো লাইব্রেরী এটিকে সহজ করে। C++-এ XML সমর্থন নিজে থেকে নেই; TinyXML-2, pugixml, অথবা Xerces এর মতো লাইব্রেরী কঠিন অংশগুলি সামলায়।

## দেখা যাক
- TinyXML-2 ডকুমেন্টেশন: https://leethomason.github.io/tinyxml2/
- pugixml লাইব্রেরী: https://pugixml.org/
- Xerces-C++ পারসার: https://xerces.apache.org/xerces-c/
- W3C XML স্পেসিফিকেশন: https://www.w3.org/XML/
