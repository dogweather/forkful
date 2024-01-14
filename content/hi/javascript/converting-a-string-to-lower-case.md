---
title:                "Javascript: स्ट्रिंग को निचले अक्षर में बदलना"
simple_title:         "स्ट्रिंग को निचले अक्षर में बदलना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्यों

कभी-कभी हमारे पास उपयोगकर्ताओं द्वारा दिए गए डेटा में थोड़ा सा अंतर होता है। स्ट्रिंग में विभिन्न अक्षरों के बीच अंतर होने से कई बार डेटा को सही तरीके से फ़ॉर्मेट करना मुश्किल हो जाता है। इसके लिए, हमें स्ट्रिंग को लोअर केस में बदलने की आवश्यकता होती है।

## कैसे करें

```javascript
var str = "HELLO WORLD";
var lowerStr = str.toLowerCase();

console.log(lowerStr);
// output: hello world
```

स्ट्रिंग को लोअर केस में बदलने के लिए, हम सिंपल एपीआई का उपयोग कर सकते हैं - `toLowerCase()`। इस सादे कदम में, स्ट्रिंग का केस स्वतः ही छोटा हो जाता है और हमें अनुचित अक्षरों का सामना नहीं करना पड़ता है।

## गहराई में जाएं

स्ट्रिंग को लोअर केस में बदलने से पहले, इसमें क्या काम होता है? इसके पीछे की प्रक्रिया क्या होती है? वास्तव में, `toLowerCase()` फ़ंक्शन एक यूनिकोड चैरेक्टर सेट के साथ काम करता है जो ज़्यादातर भाषाओं को समर्थित करता है। आप इस फ़ंक्शन को तब भी उपयोग कर सकते हैं जब कोई अपेक्षित स्ट्रिंग मामूली चिह्नों के साथ बनाई गई हो।

## और जानें

यदि आपको अभी भी स्ट्रिंग को लोअर केस में बदलने को लेकर कोई संदेह है, तो आप इन लिंक्स को देख सकते हैं:

- [MDN डॉक्यूमेंटेशन](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [W3Schools ट्यूटोरियल](https://www.w3schools.com/charsets/ref_html_utf8.asp)
- [कोड सैम्पल](https://www.geeksforge