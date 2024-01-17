---
title:                "स्ट्रिंग को कैपिटलाइज करना"
html_title:           "Javascript: स्ट्रिंग को कैपिटलाइज करना"
simple_title:         "स्ट्रिंग को कैपिटलाइज करना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
कोई भी स्ट्रिंग (तार) या शब्द जब हम कैपिटल लेटर्स (मामूली नकशे में लिखे लेटर्स) से बनाते हैं, तो हम उसे कैपिटलाइज़ (मोटे अक्षरों से लिखना) करते हैं। यह देखने में शायद असाधारण लगे, लेकिन प्रोग्रामर्स इसका इस्तेमाल उस शब्द की पहचान बढ़ाने के लिए करते हैं।

## कैसे करें?
```
वादा = "मैं वादा रहूँगा";
console.log(वादा.toUpperCase()); 
// आउटपुट: वादा Rहूँगा।
```

```
नाम = "रामचंद्र";
console.log(नाम.charAt(0).toUpperCase() + नाम.slice(1)); 
// आउटपुट: रामचंद्र।
```

## गहराई में जाएं
कैपिटलाइज़ का इतिहास बहुत ही पुराना है। यह पहले से ही टाइपिकल लेटर्स (आम नकशे वाले अक्षर) चरण का हिस्सा रहा है। इसके अलावा, इसके अन्य कई विकल्प भी हैं, जैसे कि स्ट्रिंग को मामूली अच्छी तरह से कैपिटलाइज़ किया जा सकता है और उसे प्रिंट किया जा सकता है। यह कैसे करें और कोड पर पूरी तरह से निर्भर करता है।

## इससे जुड़ी अन्य स्रोतों के लिंक
- [W3Schools पर स्ट्रिंग कैपिटलाइज़ फ़ंक्शन का उपयोग कैसे करें](https://www.w3schools.com/jsref/jsref_touppercase.asp)
- [MDN वेब डेवलपर नेटवर्क के माध्यम से Box के साथ मामूली स्ट्रिंग कैपिटलाइज़ करें](https://developer.mozilla.org/en-US/docs/Web/Javascript/Reference/Global_Objects/String/toUpperCase)
- [जावास्क्रिप्ट प्रोग्रामिंग भाषा पर कैपिटलाइज़ फंक्शन के महत्व पर एक वीडियो](https://www.youtube.com/watch?v=IOZtNx74Mnw)