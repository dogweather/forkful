---
title:                "संख्याओं को पूर्णांक बनाना"
aliases:
- /hi/javascript/rounding-numbers.md
date:                  2024-01-26T03:46:26.325656-07:00
model:                 gpt-4-0125-preview
simple_title:         "संख्याओं को पूर्णांक बनाना"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/rounding-numbers.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
संख्या में एक निश्चित बिंदु के बाद के शोर को हटा देना ही राउंडिंग है। प्रोग्रामर्स प्रेसिजन को नियंत्रित करने, मेमोरी का प्रबंधन करने, या आउटपुट को उपयोगकर्ता-अनुकूल बनाने के लिए राउंड करते हैं—जैसे 2.998 को साफ 3 में बदलना।

## कैसे:
यहाँ आप `Math.round()`, `Math.ceil()`, और `Math.floor()` का उपयोग करके JavaScript में संख्याओं को कैसे राउंड कर सकते हैं:

```javascript
let originalNumber = 2.567;

let roundedDown = Math.floor(originalNumber); // 2
let roundedUp = Math.ceil(originalNumber);    // 3
let rounded = Math.round(originalNumber);     // 3 (क्योंकि .567 है .5 से अधिक)

console.log(roundedDown); // प्रिंट करता है: 2
console.log(roundedUp);   // प्रिंट करता है: 3
console.log(rounded);     // प्रिंट करता है: 3
```

एक निश्चित संख्या की दशमलव जगहों पर सेट करने के लिए, `toFixed()` का उपयोग करें:

```javascript
let twoDecimals = originalNumber.toFixed(2); // "2.57" (एक स्ट्रिंग लौटाता है)

console.log(twoDecimals); // प्रिंट करता है: "2.57"
```

स्ट्रिंग को वापस एक संख्या में बदलने के लिए एक यूनेरी प्लस या `Number()` का उपयोग करें:

```javascript
let numberAgain = +twoDecimals; // 2.57

console.log(numberAgain); // प्रिंट करता है: 2.57
```

## गहराई में
संख्याओं का राउंडिंग नया नहीं है; यह संख्याओं के जितना पुराना है। JavaScript में, `Math.round()` "राउंड हाफ अप" टाई-ब्रेकिंग का उपयोग करता है: यदि फ्रैक्शनल भाग 0.5 है, तो यह निकटतम सम नंबर पर राउंड करता है।

अधिक नियंत्रण के लिए, `toFixed()` आपकी जाने-माने विधि हो सकती है, लेकिन याद रखें, यह एक स्ट्रिंग लौटाता है। वापस एक संख्या में परिवर्तन एक अतिरिक्त कदम हो सकता है लेकिन यह सुनिश्चित करता है कि आप न्यूमेरिक प्रकारों के साथ काम करते रहें।

विकल्प? `lodash` जैसी पुस्तकालय `_.round(number, [precision=0])` की पेशकश करती है जो अधिक सूक्ष्म नियंत्रण के लिए है। या, नवीन `Intl.NumberFormat` आपको केवल राउंडिंग से परे उच्च-प्रेसिजन फॉर्मेटिंग प्रदान करता है।

प्रेसिजन की बात करें तो, JavaScript में फ्लोटिंग-पॉइंट की विचित्रताओं के बारे में सावधान रहें। `0.1 + 0.2` बिल्कुल `0.3` के बराबर नहीं होता है, जिस तरह से संख्याएं संग्रहित होती हैं। कभी-कभी, ऐसी फ्लोटिंग-पॉइंट त्रुटियों को सही करने के लिए राउंडिंग आवश्यक हो जाती है।

## देखें भी
- मोजिला का मैथ डाॅक्यूमेंटेशन: [MDN वेब डॉक्स](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math)
- `Intl.NumberFormat` के साथ वित्तीय राउंडिंग: [ECMAScript इंटरनेशनलाइज़ेशन API](https://tc39.es/ecma402/#numberformat-objects)
- `lodash` राउंडिंग: [Lodash डॉक्स](https://lodash.com/docs/4.17.15#round)
