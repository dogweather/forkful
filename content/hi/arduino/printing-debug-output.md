---
title:                "Arduino: डीबग आउटपुट प्रिंट करना"
simple_title:         "डीबग आउटपुट प्रिंट करना"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## क्यों

Arduino प्रोग्रामिंग ब्लॉग पोस्ट को हिंदी पाठकों के लिए लिखें 

आप क्यों डिबग उत्पादन में शामिल होना चाहेंगे, इसका कारण सिर्फ 1-2 वाक्यों में बताएं।

## कैसे करें

कोड उदाहरण और सैंपल आउटपुट के साथ "Arduino ... " कोड ब्लॉक्स के भीतर समावेश करें।

```arduino
int sensorValue = analogRead(A0); //analogRead() फंक्शन का उपयोग सेंसर का मूल्य पढ़ने के लिए
Serial.print("सेंसर का मूल्य: "); //Serial.print() फंक्शन का उपयोग स्ट्रिंग का आउटपुट दिखाने के लिए
Serial.println(sensorValue); //Serial.println() फंक्शन का उपयोग नई लाइन पर मूल्य का आउटपुट दिखाने के लिए
```

इस उदाहरण में, हमने analogRead() फंक्शन से सेंसर का मूल्य पढ़ा और उसे Serial.print() और Serial.println() फंक्शन के माध्यम से सीरियल मॉनिटर में प्रिंट किया। इस तरह से डिबग उत्पादन करके हम अपने कोड में त्रुटियां ढूंढ सकते हैं और उन्हें ठीक कर सकते हैं। 

## गहराई में जाएं

डिबग उत्पादन के बारे में गहराई में जाने के लिए, यह जानना महत्वपूर्ण है कि Arduino में दो तरीके से डिबग आउटपुट प्रिंट किया जा सकता है - Serial.print() और Serial.println()। Serial.print() का उपयोग एक ही लाइन में आउटपुट दिखाने के लिए किया जाता है जबकि Serial.println() नई लाइन पर आउटपुट दिखाता है। इसके अतिरिक्त, हम सीरियल मॉनिटर के साथ बॉड दर (baud rate) भी सेट कर सकते हैं। ध्यान रखें कि बॉड दर अपने कंप्यूटर के स