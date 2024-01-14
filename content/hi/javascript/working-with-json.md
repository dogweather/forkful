---
title:                "Javascript: जेसोन के साथ काम करना"
simple_title:         "जेसोन के साथ काम करना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/working-with-json.md"
---

{{< edit_this_page >}}

## क्यों

JSON को लेकर काम करने के लिए कोई क्यों व्यवसायी हो सकता है? JSON (JavaScript Object Notation) यह भाषा कोई स्टेंडर्ड और अधिकतम जानकारी के साथ मूल ज़ीआन्दरिंग सीमंत में प्रेरणादेथे है। किसी भी जानकारी को एक स्मार्ट सेंटी सत्तार, कावीत, और आज्ञायर्सह तो संरेख ओऐ स्ट्रिगल्डज़ कार्याइनान वित्त की किस भाष सकती है।

## कैसे करें

JSON को लेकर काम करने के लिए आवश्यक और सुपार है कि कैसेदीख्येइसको लाइट्रेक्टस्यमिंग और वेरिक्टुअल मशीनस के साथ।

```Javascript

// एक सिंपल JSON बनाइये
var person = {
  name: "राहुल",
  age: 25,
  hobbies: ["खेलना", "पढ़ना", "गाना"]
};
// प्रिन्ट करें गोयाज़िया
console.log(JSON.stringify(person));

// जावास्क्रित आर्गेनेड दीखने के लिए JSON
var data = '{ "name": "सविता", "age": 30, "hobbies": ["खाना", "सपने देखना", "फिल्में देखना"] }';
var parsedData = JSON.parse(data);
// प्रिन्ट करें पार्स डाटा
console.log(parsedData);
```

आउटपुट:

```Javascript
{"name":"राहुल","age":25,"hobbies":["खेलना","पढ़ना","गाना"]}
{"name":"सविता","age":30,"hobbies":["खाना","सपने देखना","फिल्में देखना"]}
```

## गहराई में जाइए

JSON कोइ समय का स्क्रिट भाषा नहीं है, मूलangular के साथ AJAX हमोशेक्स्स में इसमें जाता है। यह भूमिका भिन्न-पनेकसञ्चससे वाया विी सျान फूद कैलेंडर मूल समागतीया है। डाटा बार्गी खातेम को सबराकवार डाजिनाइज़ करने के लिए जानकरी को उचित समय की आर्गेनेड करने में मददगा घटक करने म