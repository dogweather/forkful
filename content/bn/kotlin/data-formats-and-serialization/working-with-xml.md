---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:35:59.842285-06:00
description: "XML \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\
  \u09BE\u09B0 \u0985\u09B0\u09CD\u09A5 \u09B9\u09B2 XML \u09A1\u0995\u09C1\u09AE\u09C7\
  \u09A8\u09CD\u099F\u0997\u09C1\u09B2\u09BF \u09AA\u09BE\u09B0\u09B8\u09BF\u0982\
  , \u09A4\u09C8\u09B0\u09BF \u098F\u09AC\u0982 \u09AE\u09CD\u09AF\u09BE\u09A8\u09BF\
  \u09AA\u09C1\u09B2\u09C7\u099F \u0995\u09B0\u09BE\u2014\u098F\u0995\u099F\u09BF\
  \ \u09AE\u09BE\u09B0\u09CD\u0995\u0986\u09AA \u09AD\u09BE\u09B7\u09BE \u09AF\u09BE\
  \ \u09A1\u09C7\u099F\u09BE \u09B8\u099E\u09CD\u099A\u09AF\u09BC \u098F\u09AC\u0982\
  \ \u099F\u09CD\u09B0\u09BE\u09A8\u09CD\u09B8\u09AB\u09BE\u09B0\u09C7\u09B0 \u099C\
  \u09A8\u09CD\u09AF \u09AC\u09CD\u09AF\u09AC\u09B9\u09C3\u09A4 \u09B9\u09AF\u09BC\
  \u0964\u2026"
lastmod: '2024-03-17T18:47:44.018936-06:00'
model: gpt-4-0125-preview
summary: "XML \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\
  \u09BE\u09B0 \u0985\u09B0\u09CD\u09A5 \u09B9\u09B2 XML \u09A1\u0995\u09C1\u09AE\u09C7\
  \u09A8\u09CD\u099F\u0997\u09C1\u09B2\u09BF \u09AA\u09BE\u09B0\u09B8\u09BF\u0982\
  , \u09A4\u09C8\u09B0\u09BF \u098F\u09AC\u0982 \u09AE\u09CD\u09AF\u09BE\u09A8\u09BF\
  \u09AA\u09C1\u09B2\u09C7\u099F \u0995\u09B0\u09BE\u2014\u098F\u0995\u099F\u09BF\
  \ \u09AE\u09BE\u09B0\u09CD\u0995\u0986\u09AA \u09AD\u09BE\u09B7\u09BE \u09AF\u09BE\
  \ \u09A1\u09C7\u099F\u09BE \u09B8\u099E\u09CD\u099A\u09AF\u09BC \u098F\u09AC\u0982\
  \ \u099F\u09CD\u09B0\u09BE\u09A8\u09CD\u09B8\u09AB\u09BE\u09B0\u09C7\u09B0 \u099C\
  \u09A8\u09CD\u09AF \u09AC\u09CD\u09AF\u09AC\u09B9\u09C3\u09A4 \u09B9\u09AF\u09BC\
  \u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\u09AE\u09BE\u09B0\u09B0\u09BE\
  \ \u098F\u099F\u09BF \u0995\u09B0\u09C7 \u09A5\u09BE\u0995\u09C7 \u0995\u09BE\u09B0\
  \u09A3 \u0985\u09A8\u09C7\u0995 \u09B8\u09BF\u09B8\u09CD\u099F\u09C7\u09AE \u098F\
  \u0996\u09A8\u0993 XML \u09AB\u09B0\u09AE\u09CD\u09AF\u09BE\u099F\u09C7 \u09A4\u09A5\
  \u09CD\u09AF \u09AC\u09BF\u09A8\u09BF\u09AE\u09AF\u09BC \u0995\u09B0\u09C7, \u098F\
  \u09AC\u0982 \u09AA\u09C1\u09B0\u09A8\u09CB \u09B8\u09AE\u09B0\u09CD\u09A5\u09A8\
  \ \u098F\u09AC\u0982 \u09AC\u09BF\u09A6\u09CD\u09AF\u09AE\u09BE\u09A8 \u09AA\u09CD\
  \u09B0\u09AF\u09C1\u0995\u09CD\u09A4\u09BF\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0987\
  \u09A8\u09CD\u099F\u09BF\u0997\u09CD\u09B0\u09C7\u09B6\u09A8\u09C7\u09B0 \u099C\u09A8\
  \u09CD\u09AF \u098F\u099F\u09BF \u09AA\u09CD\u09B0\u09AF\u09BC\u09CB\u099C\u09A8\
  \u0964."
title: "XML \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
weight: 40
---

## কিভাবে:
Kotlin এ, আপনি `javax.xml.parsers` বিল্ট-ইন ব্যবহার করে পারসিং করতে পারেন:

```Kotlin
import javax.xml.parsers.DocumentBuilderFactory
import org.w3c.dom.Document

fun parseXml(xmlData: String): Document {
    val dbFactory = DocumentBuilderFactory.newInstance()
    val dBuilder = dbFactory.newDocumentBuilder()
    return dBuilder.parse(xmlData.byteInputStream())
}
```
XML ডকুমেন্টগুলি তৈরি করতে, আপনি `javax.xml.transform` ব্যবহার করতে পারেন:

```Kotlin
import javax.xml.transform.TransformerFactory
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.StreamResult
import org.w3c.dom.Document
import java.io.StringWriter

fun convertDocumentToString(doc: Document): String {
    val transformer = TransformerFactory.newInstance().newTransformer()
    val result = StringWriter()
    transformer.transform(DOMSource(doc), StreamResult(result))
    return result.toString()
}
```
একটি ডকুমেন্ট স্ট্রিং ফরম্যাটে রূপান্তরের জন্য সাধারণ আউটপুট হবে সিম্পলি আপনার XML কনটেন্ট।

## গভীর ডাইভ
XML 90 এর দশক থেকে ওয়েব এবং সফটওয়্যার ডেভেলপমেন্টের একটি মূল স্তম্ভ হয়েছে, এর পাঠ্যক্ষমতা এবং কাঠামোগত হায়ারার্কির জন্য প্রিয়। যদিও JSON এর সরলতা এবং ছোট মেসেজ আকারের জন্য ওয়েব সার্ভিসে জনপ্রিয়তা লাভ করেছে, XML এন্টারপ্রাইজ পরিবেশ, SOAP-ভিত্তিক ওয়েব সার্ভিস এবং কনফিগারেশনগুলিতে (যেমন অ্যান্ড্রয়েড লেআউট ফাইলগুলি) প্রচলিত থেকে যায়।

Kotlin/Java এর বিল্ট-ইন সুবিধাগুলি ছাড়াও XML হ্যান্ডলিংয়ের জন্য বিভিন্ন লাইব্রেরি এবং API পাওয়া যায়, যেমন Simple XML Serialization এবং Jackson XML মডিউল। কিন্তু `javax.xml.parsers` এবং `javax.xml.transform` সাধারণত বাহ্যিক নির্ভরতা ছাড়াই অধিকাংশ প্রয়োজন সেবা দেয়।

Kotlin এ XML নিয়ে কাজ করার সময়, কী বাস্তবায়নের বিবরণের মধ্যে চরিত্রের এনকোডিং যথাযথভাবে হ্যান্ডল করা এবং XML ইনজেকশন হামলাগুলি প্রতিরোধের জন্য XML এন্টিটিগুলি ম্যানেজ করা অন্তর্ভুক্ত। ডেটা অখণ্ডতা নিশ্চিত করতে XML পারসিংয়ের সময় নেমস্পেসের জটিলতা এবং স্কিমা ভ্যালিডেশনের প্রতি সচেতন থাকুন।

## আরও দেখুন
- [Kotlin ডকুমেন্টেশন](https://kotlinlang.org/docs/reference/)
- [Java DOM ডকুমেন্টেশন](https://docs.oracle.com/javase/7/docs/api/org/w3c/dom/package-summary.html)
- [Simple XML Serialization](http://simple.sourceforge.net/)
- [Jackson XML মডিউল](https://github.com/FasterXML/jackson-dataformat-xml)
