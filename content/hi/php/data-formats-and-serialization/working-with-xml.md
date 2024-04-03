---
date: 2024-01-26 04:35:11.118732-07:00
description: "\u0915\u0948\u0938\u0947: SimpleXML \u0915\u0947 \u0938\u093E\u0925\
  \ XML \u092A\u0922\u093C\u0928\u093E."
lastmod: '2024-03-13T22:44:52.520260-06:00'
model: gpt-4-0125-preview
summary: "SimpleXML \u0915\u0947 \u0938\u093E\u0925 XML \u092A\u0922\u093C\u0928\u093E\
  ."
title: "XML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\u093E"
weight: 40
---

## कैसे:
SimpleXML के साथ XML पढ़ना:

```php
$xmlString = '<?xml version="1.0" encoding="UTF-8"?>
              <note>
                <to>Tove</to>
                <from>Jani</from>
                <heading>Reminder</heading>
                <body>यह मत भूलना</body>
              </note>';
              
$xml = simplexml_load_string($xmlString);

echo $xml->to;       // प्रदर्शित होगा: Tove
echo $xml->from;     // प्रदर्शित होगा: Jani
echo $xml->heading;  // प्रदर्शित होगा: Reminder
echo $xml->body;     // प्रदर्शित होगा: यह मत भूलना
```

DOMDocument के साथ XML लिखना:

```php
$dom = new DOMDocument('1.0', 'UTF-8');

$root = $dom->createElement('note');
$dom->appendChild($root);

$to = $dom->createElement('to', 'Tove');
$from = $dom->createElement('from', 'Jani');
$heading = $dom->createElement('heading', 'Reminder');
$body = $dom->createElement('body', 'यह मत भूलना');

$root->appendChild($to);
$root->appendChild($from);
$root->appendChild($heading);
$root->appendChild($body);

echo $dom->saveXML();
```

नमूना आउटपुट:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<note>
  <to>Tove</to>
  <from>Jani</from>
  <heading>Reminder</heading>
  <body>यह मत भूलना</body>
</note>
```

## गहराई में
XML, या eXtensible Markup Language, 1998 के अपने W3C अनुशंसा के बाद से डेटा सीरियलिजेशन में एक मुख्य बिंदु रहा है। यह विस्तृत, मानव-पठनीय, और सिंटैक्स में सख्त है, जिससे यह कॉन्फिगरेशन फाइलों, डेटा इंटरचेंज, और अधिक के लिए एक विश्वसनीय विकल्प बनता है। हालांकि, इसकी सादगी और हल्के स्वभाव के कारण वेब APIs के लिए JSON द्वारा आंशिक रूप से ओवरशैडो किया गया है।

जब उन्हें XML Schemas द्वारा प्रदान की गई दस्तावेज सत्यापन की जरूरत होती है या जब वे पहले से ही इस पर भारी निर्भरता वाले इकोसिस्टम्स के भीतर काम कर रहे होते हैं (जैसे कि Microsoft Office फ़ाइल प्रारूप), तब आमतौर पर प्रोग्रामर्स XML का चयन करते हैं। PHP में SimpleXML एक्सटेंशन के साथ XML को हैंडलिंग करना सीधा है मौलिक ऑपरेशनों के लिए। अधिक जटिल मैनीपुलेशन के लिए, DOMDocument एक मजबूत फीचर सेट प्रदान करता है जो अधिक नियंत्रण की अनुमति देता है, जैसे कि नेमस्पेस हैंडलिंग और स्कीमा सत्यापन।

## देखें भी
- [PHP: SimpleXML](https://www.php.net/manual/en/book.simplexml.php)
- [PHP: DOMDocument](https://www.php.net/manual/en/class.domdocument.php)
- [W3Schools: PHP XML पार्सर्स](https://www.w3schools.com/php/php_xml_parsers.asp)
- [W3C XML स्कीमा](https://www.w3.org/XML/Schema)
