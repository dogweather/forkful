---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:36:16.256230-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: \u099C\u09BE\u09AD\u09BE DOM (\u09A1\
  \u0995\u09C1\u09AE\u09C7\u09A8\u09CD\u099F \u0985\u09AC\u099C\u09C7\u0995\u09CD\u099F\
  \ \u09AE\u09A1\u09C7\u09B2), SAX (\u09B8\u09BF\u09AE\u09CD\u09AA\u09B2 API \u09AB\
  \u09B0 XML), \u098F\u09AC\u0982 StAX (\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u09AE\
  \u09BF\u0982 API \u09AB\u09B0 XML) \u098F\u09B0 \u09AE\u09A4\u09CB API \u09B8\u09B0\
  \u09AC\u09B0\u09BE\u09B9 \u0995\u09B0\u09C7 XML \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7\
  \ \u0995\u09BE\u099C \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF\u0964 \u09A8\
  \u09BF\u099A\u09C7\u2026"
lastmod: '2024-03-17T18:47:43.930153-06:00'
model: gpt-4-0125-preview
summary: "\u099C\u09BE\u09AD\u09BE DOM (\u09A1\u0995\u09C1\u09AE\u09C7\u09A8\u09CD\
  \u099F \u0985\u09AC\u099C\u09C7\u0995\u09CD\u099F \u09AE\u09A1\u09C7\u09B2), SAX\
  \ (\u09B8\u09BF\u09AE\u09CD\u09AA\u09B2 API \u09AB\u09B0 XML), \u098F\u09AC\u0982\
  \ StAX (\u09B8\u09CD\u099F\u09CD\u09B0\u09BF\u09AE\u09BF\u0982 API \u09AB\u09B0\
  \ XML) \u098F\u09B0 \u09AE\u09A4\u09CB API \u09B8\u09B0\u09AC\u09B0\u09BE\u09B9\
  \ \u0995\u09B0\u09C7 XML \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C\
  \ \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF\u0964 \u09A8\u09BF\u099A\u09C7\
  \ \u098F\u0995\u099F\u09BF DOM \u0989\u09A6\u09BE\u09B9\u09B0\u09A3 \u09A6\u09C7\
  \u0993\u09AF\u09BC\u09BE \u09B9\u09B2 \u09AF\u09BE \u098F\u0995\u099F\u09BF XML\
  \ \u09AB\u09BE\u0987\u09B2 \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09C7."
title: "XML \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
weight: 40
---

## কিভাবে:
জাভা DOM (ডকুমেন্ট অবজেক্ট মডেল), SAX (সিম্পল API ফর XML), এবং StAX (স্ট্রিমিং API ফর XML) এর মতো API সরবরাহ করে XML এর সাথে কাজ করার জন্য। নিচে একটি DOM উদাহরণ দেওয়া হল যা একটি XML ফাইল পার্স করে:

```java
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

public class XmlParser {
    public static void main(String[] args) {
        try {
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            DocumentBuilder builder = factory.newDocumentBuilder();
            Document doc = builder.parse("data.xml");
            
            doc.getDocumentElement().normalize();
            NodeList nodeList = doc.getElementsByTagName("employee");
            
            for (int i = 0; i < nodeList.getLength(); i++) {
                Element element = (Element) nodeList.item(i);
                System.out.println("নাম: " + element.getElementsByTagName("name").item(0).getTextContent());
                System.out.println("বয়স: " + element.getElementsByTagName("age").item(0).getTextContent());
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

ধরা যাক, `data.xml` এরূপ দেখতে:

```xml
<employees>
    <employee>
        <name>Jane Doe</name>
        <age>30</age>
    </employee>
    <employee>
        <name>John Doe</name>
        <age>40</age>
    </employee>
</employees>
```

আউটপুট হবে:

```
নাম: Jane Doe
বয়স: 30
নাম: John Doe
বয়স: 40
```

## গভীর ডাইভ
XML নব্বইএর দশকের শেষ থেকে আসছে, বিভিন্ন সিস্টেমে ডেটা আদান-প্রদানের জন্য একটি গঠনমূলক এবং নমনীয় উপায় প্রদান করে। যদিও JSON এর সহজ সিনট্যাক্স এবং জাভাস্ক্রিপ্টের সাথে ঘনিষ্ঠ ইন্টিগ্রেশনের কারণে নতুন ওয়েব API এর জন্য আরও জনপ্রিয় হয়েছে, XML এন্টারপ্রাইজ পরিবেশে, SOAP-ভিত্তিক ওয়েব সার্ভিসে, এবং Microsoft Office এর Office Open XML এর মতো ডকুমেন্ট স্ট্যান্ডার্ডে ব্যাপকভাবে ব্যবহৃত হয়।

জাভাতে XML পার্সিং এর ক্ষেত্রে, DOM API ছোট ডকুমেন্টের জন্য দুর্দান্ত: এটি ট্রি-বেসড এবং মেমোরিতে XML কাঠামোতে পূর্ণ অ্যাক্সেস দেয়। তবে, বড় ফাইলের জন্য, এটি মেমোরি-ইন্টেনসিভ হতে পারে। SAX এবং StAX ইভেন্ট-চালিত এবং স্ট্রিম-ভিত্তিক যথাক্রমে হওয়ায় আরও মেমোরি-বান্ধব, তবে XML কাঠামো নেভিগেট করা অসুবিধাজনক হতে পারে।

XML তৈরি বা সংশোধনের জন্য, জাভা জাভাক্স.xml.transform এবং javax.xml.bind (JAXB) প্যাকেজও প্রদান করে। JAXB জাভা SE এর সংস্করণ 10 পর্যন্ত অংশ ছিল, তারপর থেকে জাভা SE থেকে জাভা EE মডিউল সরানোর কারণে এটি একটি পৃথক লাইব্রেরি হয়ে গেছে। এটি XML এ জাভা অবজেক্টগুলিকে সিরিয়ালাইজ করার জন্য একটি অ্যানোটেশন-চালিত উপায় এবং বিপরীতে।

## আরও দেখুন
জাভায় XML নিয়ে কাজের জন্য এই সম্পর্কিত সোর্সগুলো দেখুন:
- [জাভা API ফর XML প্রসেসিং (JAXP)](https://docs.oracle.com/javase/8/docs/technotes/guides/xml/jaxp/index.html)
- [জাভা আর্কিটেকচার ফর XML বাইন্ডিং (JAXB)](https://javaee.github.io/jaxb-v2/)
- [অরাকলের জাভাতে XML এর গাইড](https://docs.oracle.com/javase/tutorial/jaxp/index.html)
- [W3C XML প্রযুক্তি](https://www.w3.org/standards/xml/)
- [স্ট্যাক ওভারফ্লো: 'java' এবং 'xml' ট্যাগযুক্ত প্রশ্নাবলী](https://stackoverflow.com/questions/tagged/java+xml)
