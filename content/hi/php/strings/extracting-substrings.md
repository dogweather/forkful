---
date: 2024-01-20 17:46:32.158676-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) PHP \u092E\
  \u0947\u0902 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0938\
  \u0947 \u0909\u092A-\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0928\u093F\
  \u0915\u093E\u0932\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F `substr` \u092B\u0902\
  \u0915\u094D\u0936\u0928 \u090F\u0915 \u0938\u0930\u0932 \u0914\u0930 \u092A\u094D\
  \u0930\u091A\u0932\u093F\u0924 \u0924\u0930\u0940\u0915\u093E \u0939\u0948\u0964\
  ."
lastmod: '2024-04-05T21:53:54.446010-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) PHP \u092E\u0947\u0902\
  \ \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0938\u0947 \u0909\
  \u092A-\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0928\u093F\u0915\u093E\
  \u0932\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F `substr` \u092B\u0902\u0915\u094D\
  \u0936\u0928 \u090F\u0915 \u0938\u0930\u0932 \u0914\u0930 \u092A\u094D\u0930\u091A\
  \u0932\u093F\u0924 \u0924\u0930\u0940\u0915\u093E \u0939\u0948\u0964."
title: "\u0938\u092C\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0928\
  \u093F\u0915\u093E\u0932\u0928\u093E"
weight: 6
---

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
