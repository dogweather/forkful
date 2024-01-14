---
title:    "TypeScript: स्ट्रिंग को उच्च अक्षरों में लिखना"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## क्यों

उच्च उच्चारण स्ट्रिंग को प्रशस्त करना हमारे तत्वों को ढेर सारा समय बचाता है और खास तौर पर हमें दोषों से बचाने में मदद करता है।

## कैसे करें

आप उपलब्ध "toUpperCase ()" फ़ंक्शन का उपयोग कर सकते हैं जो अपने पैरामीटर को बड़े अक्षरों में प्रवर्तित करता है। नीचे दिए गए उदाहरण को देखें।

```TypeScript
var str = "hello world";
console.log(str.toUpperCase());
```

आउटपुट:
HELLO WORLD

## गहराई में खूबसूरत नजरिया

जब हम इस प्रक्रिया को गहराई से समझें, हम देखेंगे कि इस तकनीक से नहीं सिर्फ हम अपने कोड को सफेद करते हैं और उसे खास बनाते हैं, बल्कि हम बड़े अक्षरों को उस शब्द की अगार पर जोड़ कर सकते हैं। यह वाक्यांश को और अधिक असामान्य और स्पष्ट बनाता है।

## देखें भी

[JavaScript के साथ सीखें: वस्तुओं को प्रशस्त करना](https://www.freecodecamp.org/news/how-to-capitalize-words-in-javascript/)

[लफ्जों में कैपिटल कैसे करें: कई भाषाओं के साथ इस काम को करें](https://blog.usejournal.com/how-to-capitalize-words-in-multiple-languages-f1d13b0fbad7)

[पत्र तथा परिचय मेंीं पत्री माहं छं|](https://www.cs.cornell.edu/~tomf/notes/cps104/twocases.html)