---
title:                "स्ट्रिंग को मज़बूत बनाना"
html_title:           "PHP: स्ट्रिंग को मज़बूत बनाना"
simple_title:         "स्ट्रिंग को मज़बूत बनाना"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
आपने अपने PHP कोड में अक्षरों को बड़े अक्षरों में बदलते हुए देखा होगा। इस समस्या को हल करने के लिए, हम कमांड क्या हैं "Capitalizing a string" के बारे में जानेंगे। इसे प्रोग्रामर होते हैं योग्यता देने के लिए, जब वह आपके दस्तावेज़ में वेब डेवलपमेंट के दौरान सूचनाओं किया ...

## कैसे करें:
```PHP
<?php
$string = "hello world!";
echo strtoupper($string); // Output: HELLO WORLD!
```

यहाँ हमने "strtoupper ()" फ़ंक्शन का उपयोग करके स्ट्रिंग को बड़े अक्षरों में बदल दिया है। आप इस फ़ंक्शन को उसी समस्या को हल करने के लिए प्रयोग कर सकते हैं।

## गहराई में जाएँ:
बड़े अक्षरों में स्ट्रिंग को कोई नई चीज़ नहीं है। इसे पहले से ही 1978 में इंटरएक्टिव सिस्टमस कोड से लाए गए। अन्य विकल्पों के लिए, आप strtolower () फ़ंक्शन का उपयोग पा सकते हैं, जो स्ट्रिंग में सभी अक्षरों को छोटे अक्षरों में बदलता है। स्ट्रिंग को समझने के लिए आपको strtoup () फ़ंक्शन के विभिन्न संस्करणों को सुनिश्चित करने की आवश्यकता है।

## और देखें:
- https://www.php.net/manual/en/function.strtoupper.php
- https://www.php.net/manual/en/function.strtolower.php
- https://en.wikipedia.org/wiki/Character_case#Uppercase_and_lowercase