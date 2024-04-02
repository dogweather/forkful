---
date: 2024-01-20 17:46:32.158676-07:00
description: "\u0909\u092A-\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0928\
  \u093F\u0915\u093E\u0932\u0928\u093E, \u092F\u093E\u0928\u0940 \u0915\u093F\u0938\
  \u0940 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u0947 \u090F\u0915\
  \ \u092D\u093E\u0917 \u0915\u094B \u0905\u0932\u0917 \u0915\u0930\u0928\u093E\u0964\
  \ \u092F\u0939 \u0924\u092C \u091C\u0930\u0942\u0930\u0940 \u0939\u0948 \u091C\u092C\
  \ \u0915\u093F\u0938\u0940 \u0935\u093F\u0936\u0947\u0937 \u091C\u093E\u0928\u0915\
  \u093E\u0930\u0940 \u0915\u0940 \u091C\u0930\u0942\u0930\u0924 \u0939\u094B - \u091C\
  \u0948\u0938\u0947 \u092F\u0942\u091C\u0930 \u0928\u0947\u092E, \u0938\u0930\u094D\
  \u091A \u091F\u0930\u094D\u092E\u094D\u0938, \u092F\u093E \u0915\u094B\u0908\u2026"
lastmod: '2024-03-13T22:44:52.458473-06:00'
model: gpt-4-1106-preview
summary: "\u0909\u092A-\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0928\u093F\
  \u0915\u093E\u0932\u0928\u093E, \u092F\u093E\u0928\u0940 \u0915\u093F\u0938\u0940\
  \ \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u0947 \u090F\u0915 \u092D\
  \u093E\u0917 \u0915\u094B \u0905\u0932\u0917 \u0915\u0930\u0928\u093E\u0964 \u092F\
  \u0939 \u0924\u092C \u091C\u0930\u0942\u0930\u0940 \u0939\u0948 \u091C\u092C \u0915\
  \u093F\u0938\u0940 \u0935\u093F\u0936\u0947\u0937 \u091C\u093E\u0928\u0915\u093E\
  \u0930\u0940 \u0915\u0940 \u091C\u0930\u0942\u0930\u0924 \u0939\u094B - \u091C\u0948\
  \u0938\u0947 \u092F\u0942\u091C\u0930 \u0928\u0947\u092E, \u0938\u0930\u094D\u091A\
  \ \u091F\u0930\u094D\u092E\u094D\u0938, \u092F\u093E \u0915\u094B\u0908\u2026"
title: "\u0938\u092C\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0928\
  \u093F\u0915\u093E\u0932\u0928\u093E"
weight: 6
---

## What & Why? (क्या और क्यों?)
उप-स्ट्रिंग निकालना, यानी किसी स्ट्रिंग के एक भाग को अलग करना। यह तब जरूरी है जब किसी विशेष जानकारी की जरूरत हो - जैसे यूजर नेम, सर्च टर्म्स, या कोई विशिष्ट पैटर्न।

## How to: (कैसे करें:)

PHP में स्ट्रिंग्स से उप-स्ट्रिंग निकालने के लिए `substr` फंक्शन एक सरल और प्रचलित तरीका है। 

```PHP
<?php
$originalString = "नमस्ते दुनिया!";
$substring = substr($originalString, 7, 12);
echo $substring; // दुनिया!
?>
```

इस उदाहरण में, `$originalString` से `नमस्ते` के बाद का भाग, `दुनिया!`, निकाला गया है।

## Deep Dive (गहन अध्ययन):

'70 के दशक से, C प्रोग्रामिंग भाषा में `substr` जैसे फंक्शंस उपयोग में थे, और PHP ने भी इसको अपनाया। Alternatives में `mb_substr` है जो multibyte characters, जैसे हिंदी अक्षरों, के लिए उपयोगी है। `str_split` और `preg_split` जैसे फंक्शन्स भी हैं, लेकिन वे अलग काम के लिए होते हैं - पूरी स्ट्रिंग को टुकड़ों में बांटने के लिए। उप-स्ट्रिंग निकालने के लिए `substr` सबसे अधिक लचीला है: आप शुरुआत और लंबाई निर्दिष्ट कर सकते हैं।

## See Also (और भी देखें):

- PHP Manual on `substr`: [php.net/manual/en/function.substr.php](https://php.net/manual/en/function.substr.php)
- PHP Manual on `mb_substr` for multibyte strings: [php.net/manual/en/function.mb-substr.php](https://php.net/manual/en/function.mb-substr.php)
- String Functions in PHP: [php.net/manual/en/ref.strings.php](https://php.net/manual/en/ref.strings.php)
