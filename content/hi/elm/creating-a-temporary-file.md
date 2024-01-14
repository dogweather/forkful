---
title:    "Elm: एक अस्थायी फ़ाइल बनाना"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## क्यों
उन लोगों के लिए जो प्रोग्रामिंग में अथाह रूचि रखते हैं, एल्म एक उत्कृष्ट विकल्प है। अपने बेहतरीन टाइप सेवधारी, पुनः चलाने की शक्ति और आपके एप्लिकेशन को सुरक्षित बनाने में एल्म फ़ाइल सूची का उपयोग करना चाहिए।
## कैसे करें
एल्म में अस्थायी फ़ाइल बनाने के लिए, आपको `File.Extra.Temporary` मॉड्यूल पर निर्भरता होगी। यहां एक उदाहरण दिया गया है:

```
Elm

import Html exposing (text)
import File.Extra.Temporary exposing (temporaryFile)

main =
    text "अस्थायी फ़ाइल बनाने के लिए इसमें से कुछ उपयोगी हो सकता है।" 
```

यह आपको एक अस्थायी फ़ाइल बनाने का निर्देश देगा जो आपके सिस्टम के टेम्प फ़ोल्डर में बनाई जाएगी।

## गहराई में जाएं
फ़ाइल बनाने के लिए, `temporaryFile` फ़ंक्शन का उपयोग करना आसान होता है, लेकिन इसमें कई सम्भावनाएं हैं। आप पारित या अगर फ़ाइल को रद्द करने में असफल होते हैं, तो आप एक दिनांक दे सकते हैं। इसके अलावा, आप इस फ़ंक्शन का मानदंडः वर्ग या नाम, फ़ाइल एक्सटेंशन, और फ़ाइल दर्ज़ा भी निर्धारित कर सकते हैं।

अगर आप अधिक जानकारी पाना चाहते हैं, तो एल्म डॉक्यूमेंटेशन के लिए जाएं और अपने स्थानीय प्रकाशन में तालिकाओं के साथ कैसे फ़ाइलों का संग्रह करना सीखें।

## भी देखें
- [El