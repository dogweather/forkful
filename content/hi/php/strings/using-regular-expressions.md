---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:47.157503-07:00
description: "PHP \u092E\u0947\u0902 \u0928\u093F\u092F\u092E\u093F\u0924 \u0905\u092D\
  \u093F\u0935\u094D\u092F\u0915\u094D\u0924\u093F\u092F\u093E\u0901 (regex) \u0938\
  \u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u092E\u0947\u0902 \u0905\
  \u0915\u094D\u0937\u0930 \u0938\u0902\u092F\u094B\u091C\u0928\u094B\u0902 \u0915\
  \u094B \u092E\u0947\u0932 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F\
  \ \u092A\u094D\u0930\u092F\u0941\u0915\u094D\u0924 \u092A\u0948\u091F\u0930\u094D\
  \u0928 \u0939\u094B\u0924\u0940 \u0939\u0948\u0902, \u091C\u093F\u0938\u0938\u0947\
  \ \u091C\u091F\u093F\u0932 \u0916\u094B\u091C-\u0914\u0930-\u092A\u094D\u0930\u0924\
  \u093F\u0938\u094D\u0925\u093E\u092A\u0928 \u0915\u093E\u0930\u094D\u092F\u094B\u0902\
  \ \u0914\u0930\u2026"
lastmod: '2024-03-13T22:44:52.460198-06:00'
model: gpt-4-0125-preview
summary: "PHP \u092E\u0947\u0902 \u0928\u093F\u092F\u092E\u093F\u0924 \u0905\u092D\
  \u093F\u0935\u094D\u092F\u0915\u094D\u0924\u093F\u092F\u093E\u0901 (regex) \u0938\
  \u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u092E\u0947\u0902 \u0905\
  \u0915\u094D\u0937\u0930 \u0938\u0902\u092F\u094B\u091C\u0928\u094B\u0902 \u0915\
  \u094B \u092E\u0947\u0932 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F\
  \ \u092A\u094D\u0930\u092F\u0941\u0915\u094D\u0924 \u092A\u0948\u091F\u0930\u094D\
  \u0928 \u0939\u094B\u0924\u0940 \u0939\u0948\u0902, \u091C\u093F\u0938\u0938\u0947\
  \ \u091C\u091F\u093F\u0932 \u0916\u094B\u091C-\u0914\u0930-\u092A\u094D\u0930\u0924\
  \u093F\u0938\u094D\u0925\u093E\u092A\u0928 \u0915\u093E\u0930\u094D\u092F\u094B\u0902\
  \ \u0914\u0930\u2026"
title: "\u0930\u0947\u0917\u0941\u0932\u0930 \u090F\u0915\u094D\u0938\u092A\u094D\u0930\
  \u0947\u0936\u0928\u094D\u0938 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\
  \u0930\u0928\u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?

PHP में नियमित अभिव्यक्तियाँ (regex) स्ट्रिंग्स में अक्षर संयोजनों को मेल करने के लिए प्रयुक्त पैटर्न होती हैं, जिससे जटिल खोज-और-प्रतिस्थापन कार्यों और डेटा मान्यकरण की अनुमति मिलती है। प्रोग्रामर टेक्स्ट पार्सिंग, फॉर्मों के मान्यकरण, या वेब डेटा स्क्रैपिंग में इसकी शक्ति और लचीलेपन के लिए regex का उपयोग करते हैं, जिससे यह डेवलपर्स के शस्त्रागार में एक अपरिहार्य उपकरण बन जाता है।

## कैसे करें:

PHP नियमित अभिव्यक्तियों के लिए PCRE (Perl Compatible Regular Expressions) पुस्तकालय के माध्यम से समर्थन करता है, जो एक समृद्ध समूह की क्रियाओं की पेशकश करता है। यहाँ उनका उपयोग करने का तरीका है:

### एक पैटर्न का मिलान:

यदि एक स्ट्रिंग में कोई पैटर्न मौजूद है तो जांचने के लिए `preg_match()` का उपयोग करें। यदि स्ट्रिंग में पैटर्न पाया गया है तो यह फंक्शन 1 लौटाता है और नहीं तो 0 लौटाता है।

```php
if (preg_match("/\bweb\b/i", "PHP एक वेब स्क्रिप्टिंग भाषा है")) {
    echo "मिलान पाया गया।";
} else {
    echo "मिलान नहीं पाया गया।";
}
// आउटपुट: मिलान पाया गया।
```

### सभी मिलान ढूँढना:

एक स्ट्रिंग में एक पैटर्न के सभी अवसरों को ढूँढने के लिए `preg_match_all()` का उपयोग किया जाता है।

```php
$text = "बिल्लियों और कुत्ते";
$pattern = "/\b([a-z]+)\b/i";
preg_match_all($pattern, $text, $matches);
print_r($matches[0]);
// आउटपुट: Array ( [0] => बिल्लियों [1] => और [2] => कुत्ते )
```

### टेक्स्ट बदलना:

एक नियमित अभिव्यक्ति के मिलान वाले टेक्स्ट को बदलने के लिए, `preg_replace()` का उपयोग किया जाता है। यह डेटा को फॉर्मेटिंग और साफ करने के लिए अत्यंत शक्तिशाली है।

```php
$originalText = "अप्रैल 15, 2003";
$pattern = "/(\w+) (\d+), (\d+)/i";
$replacement = '${1}1,$3';
echo preg_replace($pattern, $replacement, $originalText);
// आउटपुट: अप्रैल1,2003
```

### स्ट्रिंग्स को विभाजित करना:

एक पैटर्न के लिए एक डेलिमिटर निर्दिष्ट करते हुए `preg_split()` का उपयोग करके एक स्ट्रिंग को एक ऐरे में विभाजित किया जा सकता है।

```php
$text = "PHP एक, अत्यंत लोकप्रिय, स्क्रिप्टिंग भाषा है";
$parts = preg_split("/,\s*/", $text);
print_r($parts);
// आउटपुट: Array ( [0] => PHP एक [1] => अत्यंत लोकप्रिय [2] => स्क्रिप्टिंग भाषा है )
```

इसके अतिरिक्त, जटिल regex पैटर्न और कार्यों के लिए, Symfony के `Finder` घटक या Laravel के सहायक कार्यों का संग्रह जैसे फ्रेमवर्क और पुस्तकालय एक अधिक सुविधाजनक अमूर्तन स्तर प्रदान कर सकते हैं। हालांकि, PHP स्क्रिप्टों के भीतर प्रभावी टेक्स्ट प्रसंस्करण और मान्यकरण के लिए PHP के निर्मित PCRE कार्यों को समझना और उपयोग करना महत्वपूर्ण है।
