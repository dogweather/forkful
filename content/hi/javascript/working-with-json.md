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

## क्या है और क्यों?
JSON काम करने का एक तरीका है जिसमें डेटा को आसानी से प्रस्तुत किया जाता है। प्रोग्रामर इसका उपयोग करते हैं अपने डेटा को स्टोर और ट्रांस्फर करने के लिए।

## कैसे करें?
```Javascript
// एक ऑब्जेक्ट बनाएं
var person = { "नाम": "जॉन", "उम्र": 30, "शहर": "मुंबई" };

// एक जीसन का स्ट्रिंगफाइ करें
var json = JSON.stringify(person);

// एक जीसन का पार्स कारें
var obj = JSON.parse(json);

// जीसन के फील्ड एक्सेस करें
console.log(obj.नाम);
```

आउटपुट:
```
जॉन
```

## गहराई में जाएं
JSON का उपयोग स्टैंडर्ड डेटा फॉर्मेट के रूप में 1999 में शुरू हुआ था। अन्य विकल्पों में XML और CSV शामिल हैं। आजकल, JSON उपयोगी और लोकप्रिय है क्योंकि यह अनुप्रयोगों के बीच डेटा स्ट्रिंग का आसानी से ट्रांसफर करने की अनुमति देता है। जीसन इसकी साधारण, सरल संरचना के कारण भी पसंद किया जाता है।

## और भी देखें
- [MDN का JSON आर्टिकल](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Objects/JSON)
- [कैसे करें: JavaScript में जीसन स्ट्रिंगिफाइ और पार्स](https://www.digitalocean.com/community/tutorials/how-to-work-with-json-in-javascript)