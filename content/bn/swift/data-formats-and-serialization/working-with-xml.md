---
title:                "XML এর সাথে কাজ করা"
date:                  2024-03-17T18:35:56.120095-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কী এবং কেন?

Swift-এ XML-এর সাথে কাজ করা মানে XML ডেটা পার্স করা এবং জেনারেট করা। প্রোগ্রামাররা এটা ডেটা আদান-প্রদানের জন্য করে, বিশেষ করে যখন তারা সেই সিস্টেমের সাথে ইন্টিগ্রেট করে যেখানে XML স্ট্যান্ডার্ড ফরম্যাট।

## কিভাবে:
Swift XML ডেটা পার্স করার জন্য `XMLParser` এবং `XMLDocument` প্রদান করে। এখানে একটি সন্নিবেশ রয়েছে যা একটি সিম্পল XML string পার্স করে:

```swift
import Foundation

let xmlString = """
<?xml version="1.0" encoding="UTF-8"?>
<note>
    <to>Tove</to>
    <from>Jani</from>
    <heading>Reminder</heading>
    <body>Don't forget the party on Friday!</body>
</note>
"""

if let xmlData = xmlString.data(using: .utf8) {
    let parser = XMLParser(data: xmlData)
    parser.delegate = someParserDelegate // আপনার XMLParserDelegate
    parser.parse()
}
```

আপনি `XMLDocument` ব্যবহার করে XML জেনারেটও করতে পারেন:

```swift
import Foundation

let note = XMLElement(name: "note")
let to = XMLElement(name: "to", stringValue: "Tove")
note.addChild(to)
let xmlDoc = XMLDocument(rootElement: note)

print(xmlDoc.xmlString(options: .nodePrettyPrint))
```

নমুনা আউটপুট:

```xml
<note>
  <to>Tove</to>
</note>
```

## গভীরে যাওয়া
XML, বা Extensible Markup Language, '90-এর দশকের শেষ থেকে আছে। এটি বৃবৎ কিন্তু মানব-পাঠ্যযোগ্য, যা এটিকে জটিল ডেটা কাঠামোর জন্য উপযোগী করে তোলে। Swift-এর XML পারসিং ক্ষমতা Python-এর ElementTree বা Java-এর JAXB এর মত বিস্তৃত না হলেও, বেসিক প্রয়োজনের জন্য তা কাজ করে।

নতুন সিস্টেমে JSON মত বিকল্পগুলি তাদের লাইটওয়েট এবং কম জটিল পারসারের জন্য প্রায়ই প্রাধান্য পায়, কিন্তু XML অনেক এন্টারপ্রাইজ এবং লেগাসি সিস্টেমে এখনো প্রমিনেন্ট।

Swift-এ XML-এর সাথে কাজ করার সময়, `XMLParser` একটি স্ট্রিম-ভিত্তিক পার্সার যার মানে এটি XML ডকুমেন্টের মাধ্যমে ক্রমান্বয়ভাবে পড়ে। বড় XML ফাইলগুলির জন্য, এটি মেমরি-দক্ষ। তবে, যদি আপনি সাধারণতা খুঁজছেন এবং আপনার XML ডেটা যুক্তিসঙ্গত ছোট হয়, তাহলে `XMLDocument` ব্যবহার করা আরও সোজা হতে পারে।

## আরও দেখুন
- [Apple-এর XML Parsing Guide](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/XMLParsing/XMLParsing.html)
- [W3Schools XML টিউটোরিয়াল](https://www.w3schools.com/xml/)
