---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:01.867100-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: \u091C\u0947\u0938\
  \u0928 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u090F\u0915\
  \ \u091C\u093E\u0935\u093E\u0938\u094D\u0915\u094D\u0930\u093F\u092A\u094D\u091F\
  \ \u0911\u092C\u094D\u091C\u0947\u0915\u094D\u091F \u092E\u0947\u0902 \u092A\u0930\
  \u093F\u0935\u0930\u094D\u0924\u093F\u0924 \u0915\u0930\u0928\u0947 \u0915\u0947\
  \ \u0932\u093F\u090F `JSON.parse()` \u0915\u093E \u0909\u092A\u092F\u094B\u0917\
  \ \u0915\u0930\u0947\u0902\u0964."
lastmod: '2024-03-13T22:44:53.026430-06:00'
model: gpt-4-0125-preview
summary: "\u091C\u0947\u0938\u0928 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\
  \ \u0915\u094B \u090F\u0915 \u091C\u093E\u0935\u093E\u0938\u094D\u0915\u094D\u0930\
  \u093F\u092A\u094D\u091F \u0911\u092C\u094D\u091C\u0947\u0915\u094D\u091F \u092E\
  \u0947\u0902 \u092A\u0930\u093F\u0935\u0930\u094D\u0924\u093F\u0924 \u0915\u0930\
  \u0928\u0947 \u0915\u0947 \u0932\u093F\u090F `JSON.parse()` \u0915\u093E \u0909\u092A\
  \u092F\u094B\u0917 \u0915\u0930\u0947\u0902\u0964."
title: "JSON \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u093E"
weight: 38
---

## कैसे करें:


### JSON पार्स करना
जेसन स्ट्रिंग को एक जावास्क्रिप्ट ऑब्जेक्ट में परिवर्तित करने के लिए `JSON.parse()` का उपयोग करें।

```javascript
const jsonString = '{"name":"John", "age":30, "city":"New York"}';
const obj = JSON.parse(jsonString);
console.log(obj.name); // आउटपुट: John
```

### जावास्क्रिप्ट ऑब्जेक्ट्स को स्ट्रिंगीफाई करना
जावास्क्रिप्ट ऑब्जेक्ट को वापस जेसन स्ट्रिंग में परिवर्तित करने के लिए `JSON.stringify()` का उपयोग करें।

```javascript
const user = { name: "Jane", age: 25, city: "London" };
const jsonString = JSON.stringify(user);
console.log(jsonString); // आउटपुट: {"name":"Jane","age":25,"city":"London"}
```

### Node.js में फाइल्स के साथ काम करना
Node.js वातावरण में एक JSON फाइल को पढ़ने और इसे एक ऑब्जेक्ट में परिवर्तित करने के लिए आप `fs` मॉड्यूल का उपयोग कर सकते हैं। यह उदाहरण मानता है कि आपके पास `data.json` नामक एक फाइल है।

```javascript
const fs = require('fs');

fs.readFile('data.json', 'utf-8', (err, data) => {
    if (err) throw err;
    const obj = JSON.parse(data);
    console.log(obj);
});
```

एक ऑब्जेक्ट को JSON फाइल में लिखने के लिए:

```javascript
const fs = require('fs');
const user = { name: "Mike", age: 22, city: "Berlin" };

fs.writeFile('user.json', JSON.stringify(user, null, 2), (err) => {
    if (err) throw err;
    console.log('फाइल में डेटा लिखा गया');
});
```

### तीसरे पक्ष की लाइब्रेरी
जटिल JSON ऑपरेशनों के लिए, `lodash` जैसे फ्रेमवर्क्स और लाइब्रेरीज कार्यों को सरल बना सकते हैं, लेकिन मूल ऑपरेशनों के लिए, मूल जावास्क्रिप्ट फंक्शन अक्सर पर्याप्त होते हैं। बड़े पैमाने पर या प्रदर्शन संवेदनशील एप्लीकेशनों के लिए, आप `fast-json-stringify` जैसी लाइब्रेरियों को तेजी से JSON स्ट्रिंगिफिकेशन के लिए या एक अधिक लचीले JSON प्रारूप का उपयोग करने के लिए `json5` का विचार कर सकते हैं।

`json5` के साथ पार्सिंग:
```javascript
const JSON5 = require('json5');

const jsonString = '{name:"John", age:30, city:"New York"}';
const obj = JSON5.parse(jsonString);
console.log(obj.name); // आउटपुट: John
```

ये उदाहरण जावास्क्रिप्ट में JSON के साथ मूलिक ऑपरेशनों को कवर करते हैं, जो अन्य भाषाओं से संक्रमण करने वाले नौसिखियों के लिए उत्तम हैं और जो वेब एप्लीकेशन्स में कुशलतापूर्वक डेटा संभालना चाहते हैं।
