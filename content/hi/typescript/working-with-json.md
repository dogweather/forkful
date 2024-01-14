---
title:                "TypeScript: Json के साथ काम करना"
simple_title:         "Json के साथ काम करना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/working-with-json.md"
---

{{< edit_this_page >}}

## क्यों

JSON का उपयोग डेटा को ट्रांसफर और संग्रहण करने के लिए बहुत सरल तरीके में किया जाता है और यह टाइपस्क्रिप्ट में इस्तेमाल होने वाली बहुत सारी वेब अप्लिकेशंस के लिए महत्वपूर्ण है।

## कैसे

जैसा कि स्पष्ट है, जेसॉन (JSON) एक संरचना है जो डेटा को कंप्यूटर और नेटवर्क के बीच ट्रांसफर करने के लिए उपयोग किया जाता है। टाइपस्क्रिप्ट में जेसॉन का उपयोग करने के लिए, हम `JSON.stringify() ` और `JSON.parse()` का उपयोग कर सकते हैं। नीचे दिए गए कोड ब्लॉक उदाहरण देखें:

```TypeScript
let employee = {
    name: "Rohan",
    position: "Software Engineer",
    age: 25
}

let json = JSON.stringify(employee); // Object को जेसॉन मे रूपांतरित करें
console.log(json)
// Output: {"name":"Rohan","position":"Software Engineer","age":25}

let obj = JSON.parse(json); // जेसॉन को वापस Object में रूपांतरित करें
console.log(obj)
// Output: {name: 'Rohan', position: 'Software Engineer', age: 25}
```

## गहराई में जाएं

जेसॉन कोडिंग में आप JSON स्ट्रिंग के साथ कई अंश दिखाता है, जिसमें इंटरसेप्टर, डेसियलाइजर और अन्य होते हैं। जेसॉन का उपयोग करें उन डेटा टाइप को जोड़ने के लिए जो पहले से ही नहीं हैं जैसे कि अलग-अलग डेटा जोड़, श्रृंखला, उपलब्धता और अन्य डेटा टाइप सम्बन्धित कोड सामग्री के साथ आपको समेकित कर सकता है।

## और भी देखें

"हर एक्सप्लोर करें।" दूसरे पोस्ट जैसे "जावास्क्रिप्ट के लिए जेसॉन", "जेसॉन प्रोटोकॉल", और "जेसॉन कोड