---
date: 2024-01-26 04:36:44.918323-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: Swift `XMLParser`\
  \ \u0914\u0930 `XMLDocument` \u0915\u094B XML \u0921\u0947\u091F\u093E \u092A\u093E\
  \u0930\u094D\u0938 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u092A\
  \u094D\u0930\u0926\u093E\u0928 \u0915\u0930\u0924\u093E \u0939\u0948\u0964 \u092F\
  \u0939\u093E\u0901 \u090F\u0915 \u0938\u093E\u0927\u093E\u0930\u0923 XML \u0938\u094D\
  \u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u092A\u093E\u0930\u094D\u0938\
  \ \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u090F\u0915 \u0938\u094D\
  \u0928\u093F\u092A\u0947\u091F \u0939\u0948."
lastmod: '2024-03-13T22:44:52.960117-06:00'
model: gpt-4-0125-preview
summary: "Swift `XMLParser` \u0914\u0930 `XMLDocument` \u0915\u094B XML \u0921\u0947\
  \u091F\u093E \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\u0928\u0947 \u0915\u0947\
  \ \u0932\u093F\u090F \u092A\u094D\u0930\u0926\u093E\u0928 \u0915\u0930\u0924\u093E\
  \ \u0939\u0948\u0964 \u092F\u0939\u093E\u0901 \u090F\u0915 \u0938\u093E\u0927\u093E\
  \u0930\u0923 XML \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u092A\
  \u093E\u0930\u094D\u0938 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F\
  \ \u090F\u0915 \u0938\u094D\u0928\u093F\u092A\u0947\u091F \u0939\u0948."
title: "XML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\u093E"
weight: 40
---

## कैसे करें:
Swift `XMLParser` और `XMLDocument` को XML डेटा पार्स करने के लिए प्रदान करता है। यहाँ एक साधारण XML स्ट्रिंग को पार्स करने के लिए एक स्निपेट है:

```swift
import Foundation

let xmlString = """
<?xml version="1.0" encoding="UTF-8"?>
<note>
    <to>Tove</to>
    <from>Jani</from>
    <heading>Reminder</heading>
    <body>शुक्रवार को पार्टी को मत भूलना!</body>
</note>
"""

if let xmlData = xmlString.data(using: .utf8) {
    let parser = XMLParser(data: xmlData)
    parser.delegate = someParserDelegate // आपका XMLParserDelegate
    parser.parse()
}
```

आप `XMLDocument` का उपयोग करके XML भी उत्पन्न कर सकते हैं:

```swift
import Foundation

let note = XMLElement(name: "note")
let to = XMLElement(name: "to", stringValue: "Tove")
note.addChild(to)
let xmlDoc = XMLDocument(rootElement: note)

print(xmlDoc.xmlString(options: .nodePrettyPrint))
```

नमूना आउटपुट:

```xml
<note>
  <to>Tove</to>
</note>
```

## गहराई में जानें
XML, या Extensible Markup Language, 90 के दशक के अंत से आसपास रहा है। यह वर्बोस है लेकिन मानव-पठनीय, जो इसे जटिल डेटा संरचनाओं के लिए एक अच्छा फिट बनाता है। Swift की XML पार्सिंग क्षमताएँ Python के ElementTree या Java के JAXB जितनी मजबूत नहीं हैं, लेकिन बुनियादी जरूरतों के लिए वे काम कर जाती हैं।

नए सिस्टमों में अक्सर JSON जैसे विकल्पों को हल्का वजन और कम जटिल पार्सर के कारण पसंद किया जाता है, लेकिन XML अभी भी कई उद्यम और विरासती सिस्टमों में प्रमुख है।

Swift में XML के साथ काम करते समय, `XMLParser` एक स्ट्रीम-आधारित पार्सर है जिसका अर्थ है कि यह XML दस्तावेज़ को क्रमिक रूप से पढ़ता है। बड़ी XML फाइलों के लिए, यह मेमोरी-कुशल है। हालाँकि, यदि आप सादगी की तलाश में हैं और आपका XML डेटा उचित रूप से छोटा है, तो `XMLDocument` का उपयोग करना अधिक सीधा हो सकता है।

## देखें भी
- [Apple की XML पार्सिंग गाइड](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/XMLParsing/XMLParsing.html)
- [W3Schools XML ट्यूटोरियल](https://www.w3schools.com/xml/)
