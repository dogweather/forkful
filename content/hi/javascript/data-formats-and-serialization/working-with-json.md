---
title:                "JSON के साथ काम करना"
aliases:
- /hi/javascript/working-with-json/
date:                  2024-02-03T19:24:01.867100-07:00
model:                 gpt-4-0125-preview
simple_title:         "JSON के साथ काम करना"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

JSON (जावास्क्रिप्ट ऑब्जेक्ट नोटेशन) एक हल्का डेटा-इंटरचेंज प्रारूप है, जिसे इंसानों द्वारा पढ़ना और लिखना आसान होता है और मशीनों द्वारा पार्स और जेनरेट करना भी। प्रोग्रामर्स इसका इस्तेमाल वेब एप्लीकेशन्स में डेटा को स्टोर और ट्रांसपोर्ट करने के लिए करते हैं, जिससे यह आधुनिक API और वेब सेवाओं के संचार की रीढ़ बन जाता है।

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
