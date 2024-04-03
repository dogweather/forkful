---
date: 2024-01-26 04:30:31.673395-07:00
description: "XML (eXtensible Markup Language) \u0921\u0947\u091F\u093E \u0915\u094B\
  \ \u090F\u0915 \u092A\u0920\u0928\u0940\u092F \u092A\u094D\u0930\u093E\u0930\u0942\
  \u092A \u092E\u0947\u0902 \u0938\u0902\u0930\u091A\u0928\u093E \u0926\u0947\u0928\
  \u0947 \u0915\u0947 \u092C\u093E\u0930\u0947 \u092E\u0947\u0902 \u0939\u0948\u0964\
  \ \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930 XML \u0915\u094B\
  \ \u0915\u0949\u0928\u094D\u092B\u093F\u0917\u0930\u0947\u0936\u0928, \u090F\u092A\
  \u094D\u0938 \u0915\u0947 \u092C\u0940\u091A \u0921\u0947\u091F\u093E \u0906\u0926\
  \u093E\u0928-\u092A\u094D\u0930\u0926\u093E\u0928, \u0914\u0930\u2026"
lastmod: '2024-03-13T22:44:52.373929-06:00'
model: gpt-4-0125-preview
summary: "XML (eXtensible Markup Language) \u0921\u0947\u091F\u093E \u0915\u094B \u090F\
  \u0915 \u092A\u0920\u0928\u0940\u092F \u092A\u094D\u0930\u093E\u0930\u0942\u092A\
  \ \u092E\u0947\u0902 \u0938\u0902\u0930\u091A\u0928\u093E \u0926\u0947\u0928\u0947\
  \ \u0915\u0947 \u092C\u093E\u0930\u0947 \u092E\u0947\u0902 \u0939\u0948\u0964 \u092A\
  \u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930 XML \u0915\u094B \u0915\u0949\
  \u0928\u094D\u092B\u093F\u0917\u0930\u0947\u0936\u0928, \u090F\u092A\u094D\u0938\
  \ \u0915\u0947 \u092C\u0940\u091A \u0921\u0947\u091F\u093E \u0906\u0926\u093E\u0928\
  -\u092A\u094D\u0930\u0926\u093E\u0928, \u0914\u0930 \u091C\u0939\u093E\u0902 \u0935\
  \u093F\u0936\u0947\u0937\u0924\u093E\u090F\u0902 \u0907\u0938\u0915\u0947 \u0932\
  \u093F\u090F \u0915\u0939\u0924\u0940 \u0939\u0948\u0902 - SOAP \u092F\u093E \u0935\
  \u0947\u092C APIs \u0915\u0947 \u092C\u093E\u0930\u0947 \u092E\u0947\u0902 \u0938\
  \u094B\u091A\u0947\u0902\u0964."
title: "XML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\u093E"
weight: 40
---

## कैसे करें:
```C#
using System;
using System.Xml;
using System.Xml.Linq;

class Program
{
     static void Main()
     {
        var xmlString = @"<bookstore>
                            <book>
                              <title lang=""en"">Head First C#</title>
                              <price>39.99</price>
                            </book>
                          </bookstore>";

        // इस तार को XDocument में पार्स करें
        XDocument doc = XDocument.Parse(xmlString);

        // एक नई पुस्तक जोड़ें
        doc.Element("bookstore").Add(
            new XElement("book",
                new XElement("title", "Learning XML", new XAttribute("lang", "en")),
                new XElement("price", 29.99)
            )
        );

        // कंसोल पर XML लिखें
        Console.WriteLine(doc);

        // दस्तावेज़ लोड करें
        XmlDocument xmlDoc = new XmlDocument();
        xmlDoc.LoadXml(xmlString);

        // सभी मूल्यों को पुनः प्राप्त करें
        XmlNodeList prices = xmlDoc.GetElementsByTagName("price");
        foreach (XmlNode price in prices)
        {
            Console.WriteLine(price.InnerText);
        }
     }
}

// नमूने का आउटपुट:
// <bookstore>
//  <book>
//    <title lang="en">Head First C#</title>
//    <price>39.99</price>
//  </book>
//  <book>
//    <title lang="en">Learning XML</title>
//    <price>29.99</price>
//  </book>
// </bookstore>
// 39.99
// 29.99
```

## गहरी छानबीन
देर '90 के दशक से XML का उपयोग हो रहा है, जो इसे तकनीकी वर्षों में एक दादा बनाता है। इसे डेटा पोर्टेबिलिटी और मानव पठनीयता के आसानी के लिए उपजाया गया था। अब वेब संदर्भों में, JSON जैसे विकल्प इसके पीछे चल रहे हैं क्योंकि यह हलका है और, कई लोगों के लिए, संभालने में आसान है। लेकिन XML अभी भी कई पुरानी प्रणालियों और कुछ संचार प्रोटोकॉल में अपनी जगह बनाए हुए है। XML के साथ, आपको अपनी संरचना को मान्य करने और टैग संघर्षों से बचने के लिए नेमस्पेस मिलते हैं - विशेषताएं जो इसकी उद्यम-तैयार परिपक्वता की बात करती हैं।

C# में, `System.Xml.Linq` और `System.Xml` नेमस्पेस XML के साथ काम करने के लिए दो बड़े हथियार हैं। LINQ to XML (`XDocument`, `XElement`) अधिक आधुनिक और अधिक सुरुचिपूर्ण है - आपने इसका जादू उदाहरण में देखा है। `XmlDocument` आपको DOM (Document Object Model) दृष्टिकोण देता है - थोड़ा पुराना स्कूल, लेकिन कुछ लोग इसकी शक्ति की कसम खाते हैं।

## देखें भी
- [MSDN – LINQ to XML का ओवरव्यू](https://docs.microsoft.com/dotnet/standard/linq/linq-xml-overview)
- [MSDN – XML डॉक्यूमेंट ऑब्जेक्ट मॉडल (DOM)](https://docs.microsoft.com/dotnet/standard/data/xml/)
- [W3Schools – XML सीखें](https://www.w3schools.com/xml/)
- [XML vs. JSON](https://www.json.org/xml.html)
