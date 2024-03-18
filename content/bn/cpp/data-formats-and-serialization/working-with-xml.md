---
title:                "XML নিয়ে কাজ করা"
date:                  2024-03-17T18:31:37.680242-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কি এবং কেন?

XML-এর সাথে কাজ করা মানে XML (eXtensible Markup Language) ডেটা পার্স করা, তৈরি করা, এবং ম্যানিপুলেট করা। এর প্ল্যাটফর্ম-নিরপেক্ষ প্রকৃতির কারণে প্রোগ্রামাররা স্ট্রাকচার্ড ডেটা ইন্টারচেঞ্জ, কনফিগারেশন, এবং আরও অনেক কিছু সামলানোর জন্য XML ম্যানেজ করে।

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
