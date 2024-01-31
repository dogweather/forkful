---
title:                "JSON के साथ काम करना"
date:                  2024-01-19
html_title:           "Arduino: JSON के साथ काम करना"
simple_title:         "JSON के साथ काम करना"

category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
JSON, या JavaScript Object Notation, एक डेटा स्वरूप है, जिसका उपयोग डेटा को आसानी से पढ़े और लिखे जाने वाले पाठ प्रारूप में संग्रहित और आदान-प्रदान करने के लिए किया जाता है. प्रोग्रामर इसका उपयोग वेब APIs से डेटा प्राप्त करने, कॉन्फ़िगरेशन फाइलों को पढ़ने, और विभिन्न भाषाओँ के बीच डेटा को आसानी से साझा करने के लिए करते हैं.

## How to: (कैसे करें:)
### JSON ऑब्जेक्ट को स्ट्रिंग में परिवर्तित करना:
```TypeScript
let userData = {
  name: 'Amit',
  age: 30,
  isAdmin: false
};

let jsonString = JSON.stringify(userData);
console.log(jsonString); // {"name":"Amit","age":30,"isAdmin":false}
```

### JSON स्ट्रिंग को ऑब्जेक्ट में परिवर्तित करना:
```TypeScript
let jsonString = '{"name":"Amit","age":30,"isAdmin":false}';
let userData = JSON.parse(jsonString);
console.log(userData.name); // Amit
```

## Deep Dive (गहराई में जानकारी):
JSON का इस्तेमाल पहली बार 2001 में एक JavaScript प्रोग्रामिंग भाषा स्वरूप के रूप में किया गया था. यह XML का एक सरल, अधिक पठनीय विकल्प है और कई प्रोग्रामिंग भाषाओं और प्लेटफॉर्म्स द्वारा समर्थन की जाती है. टाइपस्क्रिप्ट में JSON के साथ काम करते समय, `JSON.stringify()` और `JSON.parse()` विधियों का उपयोग होता है. डेटा की सटीकता और टाइप सुरक्षा के लिए, ऑब्जेक्ट और इंटरफेसेस की परिभाषाएँ बनाई जाती हैं.

## See Also (और भी देखें):
- [MDN Web Docs: Working with JSON](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Objects/JSON)
- [TypeScript: Handbook](https://www.typescriptlang.org/docs/)
- [JSON: Official Site](https://www.json.org/json-en.html)
