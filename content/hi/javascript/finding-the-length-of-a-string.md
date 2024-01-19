---
title:                "स्ट्रिंग की लंबाई पता करना"
html_title:           "C++: स्ट्रिंग की लंबाई पता करना"
simple_title:         "स्ट्रिंग की लंबाई पता करना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
"स्ट्रिंग की लंबाई नापना" यानि किसी टेक्स्ट सन्दर्भ के अक्षरों की संख्या पता करना। प्रोग्रामर्स इसे अक्सर लूपिंग में किसी विशेष अक्षर को तलाशने, या विभिन्न स्ट्रिंग्स को तुलना करने के लिए करते हैं।

## कैसे:
जावास्क्रिप्ट में, स्ट्रिंग की लंबाई का पत्लाई करने के लिए `.length` प्रॉपर्टी का इस्तेमाल करते हैं।

```Javascript
var text = "नमस्ते दुनिया";
console.log(text.length);  // ११
```

इसके आउटपुट में `११` अर्थात टेक्स्ट में सम्मिलित अक्षरों की संख्या होगी। 

## गहराई में:
1. **ऐतिहासिक प्रसंग:** जावास्क्रिप्ट में इसका उपयोग पीछे ही कुछ संस्करणों से सुरु हुआ है। पहले, यह केवल `for` लूप और `.charAt()` मेतोड के माध्यम से संभव था। 
2. **विकल्प:** कुछ केसेस में, आप `Array.from()` मेतोड का उपयोग कर सकते हैं। 
```Javascript
console.log(Array.from("नमस्ते दुनिया").length); // ११
```
3. **कार्यान्वयन विवरण:** `.length` प्रॉपर्टी बाइनरी डेटा के हर byte की length को संगणक करती है। यदि आपका टेक्स्ट unicode characters (जैसे कि एमोजी या complex scripts) में है, तो यह उनकी परिवर्तन क्षमता को चालू करता है।

## अधिक जानकारी के लिए:
1. [MDN String.length](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
2. [W3Schools String methods](https://www.w3schools.com/jsref/jsref_obj_string.asp)