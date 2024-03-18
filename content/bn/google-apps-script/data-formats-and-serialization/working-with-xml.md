---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:36:23.822707-06:00
description: "Google Apps Script \u098F XML \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7\
  \ \u0995\u09BE\u099C \u0995\u09B0\u09BE \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\
  \u09BE\u09AE\u09BE\u09B0\u09A6\u09C7\u09B0\u0995\u09C7 XML \u09A1\u09C7\u099F\u09BE\
  \ \u09AA\u09BE\u09B0\u09CD\u09B8, \u09AE\u09A1\u09BF\u09AB\u09BE\u0987 \u098F\u09AC\
  \u0982 \u099C\u09C7\u09A8\u09BE\u09B0\u09C7\u099F \u0995\u09B0\u09A4\u09C7 \u0985\
  \u09A8\u09C1\u09AE\u09A4\u09BF \u09A6\u09C7\u09AF\u09BC, \u09AF\u09BE \u0993\u09AF\
  \u09BC\u09C7\u09AC \u09B8\u09C7\u09AC\u09BE \u098F\u09AC\u0982 \u0995\u09A8\u09AB\
  \u09BF\u0997\u09BE\u09B0\u09C7\u09B6\u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF\u2026"
lastmod: '2024-03-17T18:47:43.552498-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script \u098F XML \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\
  \u09BE\u099C \u0995\u09B0\u09BE \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\
  \u09AE\u09BE\u09B0\u09A6\u09C7\u09B0\u0995\u09C7 XML \u09A1\u09C7\u099F\u09BE \u09AA\
  \u09BE\u09B0\u09CD\u09B8, \u09AE\u09A1\u09BF\u09AB\u09BE\u0987 \u098F\u09AC\u0982\
  \ \u099C\u09C7\u09A8\u09BE\u09B0\u09C7\u099F \u0995\u09B0\u09A4\u09C7 \u0985\u09A8\
  \u09C1\u09AE\u09A4\u09BF \u09A6\u09C7\u09AF\u09BC, \u09AF\u09BE \u0993\u09AF\u09BC\
  \u09C7\u09AC \u09B8\u09C7\u09AC\u09BE \u098F\u09AC\u0982 \u0995\u09A8\u09AB\u09BF\
  \u0997\u09BE\u09B0\u09C7\u09B6\u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF\u2026"
title: "XML \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?

Google Apps Script এ XML এর সাথে কাজ করা প্রোগ্রামারদেরকে XML ডেটা পার্স, মডিফাই এবং জেনারেট করতে অনুমতি দেয়, যা ওয়েব সেবা এবং কনফিগারেশনের জন্য অপরিহার্য। প্রোগ্রামাররা পুরানো সিস্টেমের সাথে ইন্টিগ্রেট করতে, ওয়েব স্ক্র্যাপিং করতে, অথবা অসংখ্য API এর সাথে যোগাযোগ করতে এই পদ্ধতি গ্রহণ করে, যেগুলি ডেটা ইন্টারচেঞ্জের জন্য এখনও JSON এর উপর XML এ নির্ভর করে।

## কিভাবে:

Google Apps Script XML ডেটার সাথে কাজ করার জন্য `XmlService` প্রদান করে। নিম্নে আমরা কিভাবে XML স্ট্রিং পার্স, এর বিষয়বস্তু মডিফাই এবং একটি নতুন XML স্ট্রিং জেনারেট করা যায় তা দেখাচ্ছি।

XML স্ট্রিং পার্স করা:

```javascript
function parseXML() {
  var xmlString = '<root><child name="first">Hello</child><child name="second">World</child></root>';
  var document = XmlService.parse(xmlString);
  var root = document.getRootElement();
  var children = root.getChildren('child');
  Logger.log(children[0].getText()); // Logs: Hello
}
```

XML মডিফাই করতে, আপনি হয়তো একটি নতুন শিশু উপাদান যোগ করতে চাইবেন:

```javascript
function addNewChild() {
  var xmlString = '<root><child name="first">Hello</child></root>';
  var document = XmlService.parse(xmlString);
  var root = document.getRootElement();
  
  var newChild = XmlService.createElement('child').setText('World');
  root.addContent(newChild);
  
  var xml = XmlService.getPrettyFormat().format(document);
  Logger.log(xml);
  // Logs the new XML string with the added child element
}
```

শুরু থেকে XML স্ট্রিং জেনারেট করা:

```javascript
function createXML() {
  var root = XmlService.createElement('root');
  var child = XmlService.createElement('child').setText('Hello World');
  root.addContent(child);
  
  var xml = XmlService.getPrettyFormat().format(XmlService.createDocument(root));
  Logger.log(xml);
  // Outputs: <root><child>Hello World</child></root>
}
```

## গভীর ডুব

ঐতিহাসিকভাবে, XML (Extensible Markup Language) ডেটা ইন্টারচেঞ্জের জন্য ডিফ্যাক্টো মান ছিল, যতক্ষণ না JSON হালকা বিকল্প হিসেবে উত্থান পেয়েছিল। XML এর বিস্তারিত সিনট্যাক্স এবং কঠোর পার্সিং মডেল একটি শক্তিশালী, যদিও ভারী, ডেটা ফর্ম্যাট প্রদান করেছিল। Google Apps Script এ, `XmlService` API তৈরি, পার্স এবং XML ডেটার ম্যানিপুলেশন এনক্যাপসুলেট করে, বিভিন্ন লিগ্যাসি এবং এন্টারপ্রাইজ সিস্টেম, SOAP ওয়েব সার্ভিসেস, এবং অ্যাপ্লিকেশনের কনফিগারেশন ফাইলের জন্য এর চলমান গুরুত্ব স্বীকার করে।

জাভাস্ক্রিপ্টের সাথে এর সাদাসিধে সম্পর্ক ও ব্যবহারের সুবিধার জন্য আধুনিক ওয়েব ডেভেলপমেন্টে JSON প্রচলিত থাকলেও, নথির বৈধীকরণ এবং গঠনমূলক হায়ারার্কি যেখানে মুখ্য, সেখানে XML এখনও প্রাসঙ্গিক থাকে। তবে, নতুন প্রকল্পগুলির জন্য, বিশেষত ওয়েব APIs এর দিকে ঝুঁকে থাকা অবস্থায়, এর হালকা প্রকৃতি এবং জাভাস্ক্রিপ্টের সাথে সম্পন্ন সংযোগের কারণে JSON প্রায়ই আরো ব্যাবহারিক বিকল্প হয়ে ওঠে।

Google Apps Script এ XML এর বুঝ এবং এর সম্পর্ক জানা পুরানো সিস্টেম বা নির্দিষ্ট এন্টারপ্রাইজ APIs এর সাথে ইন্টিগ্রেশনে কাজ করা ডেভেলপারদের জন্য অপরিহার্য। তবে, নতুন প্রকল্প শুরু করা বা যখন নমনীয়তা মুখ্য, তখন JSON এর মতো বিকল্পের উপর XML এর প্রয়োজনীয়তা মূল্যায়ন করা উচিত।
