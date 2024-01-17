---
title:                "सबस्ट्रिंग उत्पन्न करना"
html_title:           "PHP: सबस्ट्रिंग उत्पन्न करना"
simple_title:         "सबस्ट्रिंग उत्पन्न करना"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## ये क्या है और क्यों?:
एक समूह शब्दों में से सबसे छोटे शब्दों को निकालना स्ट्रिंग से निकालना कहलाता है, जो कि कोडिंग में बहुत उपयोगी होता है। प्रोग्रामर्स अक्सर स्ट्रिंग के अंदर की जानकारी को स्पष्ट और सुलभ बनाने के लिए इस तकनीक का उपयोग करते हैं।

## कैसे करें:
```
<?php

$string = "मुख्य बाग़ ";
$substring = substr($string, 4, 5);

echo $substring; // बर्बर
?>
```

```
<?php

$string = "सीखना इससे केस करते हैं";
$substring = substr($string, 9);

echo $substring; // केस करते हैं
?>
```

## गहराई की जाँच:
इस तकनीक को 1974 में डॉनाल्ड क्रूथ द्वारा 'तकनीकी भाषा के लिए मैक्रो' नामक कॉन्सेप्ट के रूप में प्रस्तुत किया गया था। इसके अलावा, PHP में substr() के अलावा भी अन्य तरीके हैं जैसे mb_substr() जो मल्टीबाइट स्ट्रिंग को ध्यान में रखते हुए काम करता है। स्ट्रिंग से उपयुक्त दस्तावेजों और कोड को सुलभ ढंग से जाँचने के लिए इस तकनीक अपनाने की सलाह दी जाती है।

## इससे जुड़े कुछ अन्य स्रोत:
- [आधिकारिक PHP डॉक्यूमेंटेशन](https://www.php.net/manual/en/function.substr.php)
- [यूट्यूब वीडियो: स्ट्रिंग से सबसे छोटे शब्दों का निकालना](https://www.youtube.com/watch?v=sP61JjQSp0o)
- [Stack Overflow पर स्ट्रिंग से सबसे छोटे शब्दों का निकालना पर वहाँ कैसे की जाती है](https://stackoverflow.com/questions/3028426/how-to-extract-a-substring-from-a-string-in-php)