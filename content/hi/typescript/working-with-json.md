---
title:                "Json के साथ काम करना"
html_title:           "TypeScript: Json के साथ काम करना"
simple_title:         "Json के साथ काम करना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/working-with-json.md"
---

{{< edit_this_page >}}

# क्या और क्यों? 

JSON काम क्या है और यह क्यों काम किया जाता है, इसे कैसे करते हैं? कोडरों को इसे क्यों करने के लिए?

## कैसे किया जाए: 

अगर आप कोडिंग ज्ञान रखते हैं, तो आपने शायद JSON शब्द के बारे में सुना होगा। यह एक साधारण फॉर्मेट है जो डेटा को कंप्यूटर और वेब अप्प्लिकेशंसों के बीच भेजने में मदद करता है। निम्नलिखित उदाहरण में, हम TS द्वारा JSON फ़ाइल को पढ़ते हैं और उसमें से कुछ डेटा प्रिंट करते हैं।

```TypeScript
let jsonData = '{"name":"John", "age":30, "city":"New York"}';

// पार्स करें और डेटा लोड करें
let obj = JSON.parse(jsonData);

console.log("नाम: " + obj.name);
console.log("आयु: " + obj.age);
console.log("शहर: " + obj.city);
```

आउटपुट: 

```
नाम: John
आयु: 30
शहर: New York
```

## गहराई में जाएं:

JSON, जो ज़ोन इस्तीमाल करता है, एक सिर्फ वणिज्य से अधिक फॉर्मेट है। इसे कंप्यूटर आपस में डेटा को भेजने के लिए नमूना तरीकों के साथ खोजना समय होगा। यह जानना बेहद उपयोगी हो सकता है। कोडिंग के और डेटा साथ, अन्य फॉर्मेटिंग टूल के अलावा JSON का उपयोग किया जा सकता है, जैसे कि CSV, XML, और बहुत कुछ। JSON खोजने या बनाने के दौरान, आप अपना कोड दुगना प्रयोग कर मदद दैख सकते हैं।

## और भी देखें:

- [JSON का आधिकारिक साइट] (http://json.org/)
- [JSON को जड़ से सीखे ](https://www.w3schools.com/js/js_json_intro.asp)
- [अतिरिक्त रिसोर्सेज] (https://www.codecademy.com/learn/learn-json)