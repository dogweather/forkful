---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:36:26.847501-06:00
description: "XML \u09B9\u09B2 \u098F\u0995\u099F\u09BF \u09AE\u09BE\u09B0\u09CD\u0995\
  \u0986\u09AA \u09AD\u09BE\u09B7\u09BE \u09AF\u09BE \u09A1\u09C7\u099F\u09BE \u09B8\
  \u099E\u09CD\u099A\u09AF\u09BC \u098F\u09AC\u0982 \u09AA\u09B0\u09BF\u09AC\u09B9\
  \u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09AC\u09CD\u09AF\u09AC\u09B9\u09C3\
  \u09A4 \u09B9\u09AF\u09BC\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\
  \u09AE\u09BE\u09B0\u09B0\u09BE \u0985\u09CD\u09AF\u09BE\u09AA\u09CD\u09B2\u09BF\u0995\
  \u09C7\u09B6\u09A8 \u098F\u09AC\u0982 \u09B8\u09BF\u09B8\u09CD\u099F\u09C7\u09AE\
  \u09C7\u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7 \u0987\u09A8\u09CD\u099F\u09BE\u09B0\
  \u0985\u09AA\u09BE\u09B0\u09C7\u09AC\u09BF\u09B2\u09BF\u099F\u09BF \u09B8\u0995\u09CD\
  \u09B7\u09AE \u0995\u09B0\u09A4\u09C7 - \u09A1\u09C7\u099F\u09BE\u2026"
lastmod: '2024-03-17T18:47:44.158559-06:00'
model: gpt-4-0125-preview
summary: "XML \u09B9\u09B2 \u098F\u0995\u099F\u09BF \u09AE\u09BE\u09B0\u09CD\u0995\
  \u0986\u09AA \u09AD\u09BE\u09B7\u09BE \u09AF\u09BE \u09A1\u09C7\u099F\u09BE \u09B8\
  \u099E\u09CD\u099A\u09AF\u09BC \u098F\u09AC\u0982 \u09AA\u09B0\u09BF\u09AC\u09B9\
  \u09A8\u09C7\u09B0 \u099C\u09A8\u09CD\u09AF \u09AC\u09CD\u09AF\u09AC\u09B9\u09C3\
  \u09A4 \u09B9\u09AF\u09BC\u0964 \u09AA\u09CD\u09B0\u09CB\u0997\u09CD\u09B0\u09BE\
  \u09AE\u09BE\u09B0\u09B0\u09BE \u0985\u09CD\u09AF\u09BE\u09AA\u09CD\u09B2\u09BF\u0995\
  \u09C7\u09B6\u09A8 \u098F\u09AC\u0982 \u09B8\u09BF\u09B8\u09CD\u099F\u09C7\u09AE\
  \u09C7\u09B0 \u09AE\u09A7\u09CD\u09AF\u09C7 \u0987\u09A8\u09CD\u099F\u09BE\u09B0\
  \u0985\u09AA\u09BE\u09B0\u09C7\u09AC\u09BF\u09B2\u09BF\u099F\u09BF \u09B8\u0995\u09CD\
  \u09B7\u09AE \u0995\u09B0\u09A4\u09C7 - \u09A1\u09C7\u099F\u09BE\u2026"
title: "XML \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
---

{{< edit_this_page >}}

## কি এবং কেন?
XML হল একটি মার্কআপ ভাষা যা ডেটা সঞ্চয় এবং পরিবহনের জন্য ব্যবহৃত হয়। প্রোগ্রামাররা অ্যাপ্লিকেশন এবং সিস্টেমের মধ্যে ইন্টারঅপারেবিলিটি সক্ষম করতে - ডেটা বিনিময় এবং কনফিগারেশন সেটিংসের কথা ভেবে - XML নিয়ে কাজ করেন।

## কিভাবে:
SimpleXML দিয়ে XML পড়া:

```php
$xmlString = '<?xml version="1.0" encoding="UTF-8"?>
              <note>
                <to>Tove</to>
                <from>Jani</from>
                <heading>Reminder</heading>
                <body>Don't forget this</body>
              </note>';
              
$xml = simplexml_load_string($xmlString);

echo $xml->to;       // প্রদর্শন করবে: Tove
echo $xml->from;     // প্রদর্শন করবে: Jani
echo $xml->heading;  // প্রদর্শন করবে: Reminder
echo $xml->body;     // প্রদর্শন করবে: Don't forget this
```

DOMDocument দিয়ে XML লেখা:

```php
$dom = new DOMDocument('1.0', 'UTF-8');

$root = $dom->createElement('note');
$dom->appendChild($root);

$to = $dom->createElement('to', 'Tove');
$from = $dom->createElement('from', 'Jani');
$heading = $dom->createElement('heading', 'Reminder');
$body = $dom->createElement('body', 'Don't forget this');

$root->appendChild($to);
$root->appendChild($from);
$root->appendChild($heading);
$root->appendChild($body);

echo $dom->saveXML();
```

নমুনা আউটপুট:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<note>
  <to>Tove</to>
  <from>Jani</from>
  <heading>Reminder</heading>
  <body>Don't forget this</body>
</note>
```

## গভীর ডুব
XML, বা এক্সটেনসিবল মার্কআপ ভাষা, 1998 সালে W3C এর সুপারিশের পর থেকে ডেটা সিরিয়ালাইজেশনে একটি স্ট্যাপল হয়ে আছে। এটি বেশি কথা বলে, মানুষের পড়ার উপযুক্ত, এবং সিনট্যাক্সে কঠোর, যা এটিকে কনফিগারেশন ফাইল, ডেটা ইন্টারচেঞ্জ, এবং আরও অনেক কিছুর জন্য একটি নির্ভরযোগ্য বিকল্প করে তোলে। তবে, এটি তার সরলতা এবং হালকা-ওজনের স্বভাবের কারণে ওয়েব APIs-এর জন্য JSON দ্বারা আংশিকভাবে অতিক্রান্ত হয়েছে।

প্রোগ্রামাররা প্রায়শই XML বেছে নেন যখন তারা XML Schemas কর্তৃক প্রদত্ত ডকুমেন্ট ভ্যালিডেশনের প্রয়োজনে হয় বা যখন তারা ইতিমধ্যেই এর উপর ভারীভাবে নির্ভর করে এমন ইকোসিস্টেমের মধ্যে কাজ করেন (যেমন মাইক্রোসফট অফিস ফাইল ফর্ম্যাট)। PHP-তে SimpleXML এক্সটেনশনের সাথে বেসিক অপারেশনের জন্য XML হ্যান্ডলিং সোজা। আরও জটিল ম্যানিপুলেশনের জন্য, DOMDocument নামস্পেস হ্যান্ডলিং এবং স্কিমা ভ্যালিডেশনের মতো বৃহত্তর নিয়ন্ত্রণের অনুমতি দেয় এমন শক্তিশালী ফিচারের একটি রোবাস্ট সেট প্রদান করে।

## আরও দেখুন
- [PHP: SimpleXML](https://www.php.net/manual/en/book.simplexml.php)
- [PHP: DOMDocument](https://www.php.net/manual/en/class.domdocument.php)
- [W3Schools: PHP XML পার্সার](https://www.w3schools.com/php/php_xml_parsers.asp)
- [W3C XML স্কিমা](https://www.w3.org/XML/Schema)
