---
title:                "Javascript: स्ट्रिंग कैपिटलाइज करना"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्यों

कोई भी मानव भाषा में कोष्टक अक्षरों को उच्च वर्णमाला अक्षरों में रूपांतरित करना चाहेगा। यह एक ठीक से संरचित प्रोग्राम लिखने और आपकी कोई भी आवश्यकताओं को पूरा करने का सुनहरा मौका है। जावास्क्रिप्ट लैंग्वेज में, `toUpperCase()` फ़ंक्शन का इस्तेमाल अपनी कोडिंग कार्य में निकालने का सरल और स्थायी तरीका है।

## कैसे

```javascript
//अक्षरण में फंक्शन का उपयोग
let name = "हिन्दी";
let capitalized = name.toUpperCase(); // एक्षन के परिणाम को खरीदें "हिन्दी" ही खारिज
console.log(capitalized); // हिन्दी
```

```javascript
// असामान्य चारित्र को उच्च वर्णमाला के साथ कनेक्टिंग
let song = "कुछ ऐसा ही होता है (अलग खेल होता है)";
let capitalizedSong = song.toUpperCase();
console.log(capitalizedSong); // कुछ ऐसा ही होता है (अलग खेल होता है)
```

## गहराई

जब आप इस फ़ंक्शन को इस्तेमाल करते हैं, तो यह `toUpperCase()` फ़ंक्शन की आत्मा कोल करता है जो इसे इंस्टेन्ट्स से बनाता है और उसे स्ट्रिंग को कैप्स के साथ रिटर्न करता है। एक बार रिटर्न करने के बाद, प्रसंस्करण कंपोनेंट फ़ंक्शन को काम से निकाल दिया जाता है।

## देखें भी

- [JavaTPoint: जावास्क्रिप्ट अक्षरों को उच्च आवृत्ति में परिवर्तित करने के सबसे आसान तरीके](https://www.javatpoint.com/javascript-string-touppercase)
- [GeeksforGeeks: जावास्क्रिप्ट रीटर्न स्ट्रिंग को अपर करने के तरीके रिवर्स](https://www.geeksforgeeks.org/javascript-return-string-reverse-uppercase-letter-alternate/)
- [नोट स्कूल: जावास्क्र