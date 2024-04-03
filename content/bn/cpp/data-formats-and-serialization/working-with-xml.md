---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:31:37.680242-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: TinyXML-2 \u09B2\u09BE\u0987\u09AC\
  \u09CD\u09B0\u09C7\u09B0\u09BF \u09AC\u09CD\u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\
  \u09B0\u09C7 XML \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09BE\u09B0 \u098F\u0995\
  \u099F\u09BF \u09B8\u09B9\u099C \u0989\u09AA\u09BE\u09AF\u09BC \u098F\u0996\u09BE\
  \u09A8\u09C7."
lastmod: '2024-03-17T18:47:44.389679-06:00'
model: gpt-4-0125-preview
summary: "TinyXML-2 \u09B2\u09BE\u0987\u09AC\u09CD\u09B0\u09C7\u09B0\u09BF \u09AC\u09CD\
  \u09AF\u09AC\u09B9\u09BE\u09B0 \u0995\u09B0\u09C7 XML \u09AA\u09BE\u09B0\u09CD\u09B8\
  \ \u0995\u09B0\u09BE\u09B0 \u098F\u0995\u099F\u09BF \u09B8\u09B9\u099C \u0989\u09AA\
  \u09BE\u09AF\u09BC \u098F\u0996\u09BE\u09A8\u09C7."
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
