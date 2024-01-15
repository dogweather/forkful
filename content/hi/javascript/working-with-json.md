---
title:                "json के साथ काम करना"
html_title:           "Javascript: json के साथ काम करना"
simple_title:         "json के साथ काम करना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/working-with-json.md"
---

{{< edit_this_page >}}

## क्यों

जेसन (JSON) प्रोग्रामिंग में एक प्रचलित डेटा संरचना है जो डेटा को स्टोर और ट्रांस्फर करने के लिए इस्तेमाल की जाती है। यह सरल और लाइटवेट स्ट्रक्चर के साथ बहुत ही अनुकूलनीय है जो कि जावास्क्रिप्ट वेब डेवलपमेंट में आवश्यक है।

## कैसे

"```Javascript
// एक JSON ऑब्जेक्ट बनाएं
const student = {
  name: "राहुल",
  age: 25,
  course: "वेब डेवलपमेंट",
  college: "दिल्ली यूनिवर्सिटी"
};

// JSON को स्ट्रिंग में परिवर्तित करें
const studentString = JSON.stringify(student);
console.log(studentString); // {"name":"राहुल","age":25,"course":"वेब डेवलपमेंट","college":"दिल्ली यूनिवर्सिटी"}

// JSON स्ट्रिंग को पार्स करें और ऑब्जेक्ट में वापस लाएं
const studentObject = JSON.parse(studentString);
console.log(studentObject); // { name: "राहुल", age: 25, course: "वेब डेवलपमेंट", college: "दिल्ली यूनिवर्सिटी"}
"
```

## गहराई पर जाएं

- JSON को समझने के लिए डेटा टाइपिंग के साथ पिछ्ले डेटा संरचनाओं की तुलना करें, जैसे XML या CSV।
- किसी भी होस्ट सर्वर से डेटा को बेहतर तरीके से प्राप्त करने के लिए JSON प्रोटोकॉल का उपयोग करें।
- जेसन की उपयोगिता को और बढ़ाने के लिए जावास्क्रिप्ट परिवर्तन ट्रांसफर्मेशन (JSONPT) का उपयोग करें।

## देखें भी

- [JSON डॉक्यूमेंटेशन ( जावास्क्रिप्ट)](https://developer.mozilla.org/hi/docs/Learn/JavaScript/Objects/JSON)
- [JSON गाइड (जावास्क्रिप्ट)](https://www.json.org/json-en.html)
- [जेसन और जावास्क्रिप्ट उदाहरणों के साथ: संचार और बाधाएं](https://medium.com/@tkssharma/json-in-javascript-68f5c0f9c0d0)