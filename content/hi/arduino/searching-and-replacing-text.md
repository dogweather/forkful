---
title:                "पाठ की खोज और प्रतिस्थापन"
html_title:           "Bash: पाठ की खोज और प्रतिस्थापन"
simple_title:         "पाठ की खोज और प्रतिस्थापन"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
खोजकर पाठ को बदलना इसका मतलब है कि आप एक प्रकार की पाठ स्ट्रिंग को दुसरे पाठ स्ट्रिंग से बदल रहे हैं। प्रोग्रामर इसे तब करते हैं जब उन्हें कोड में दुहराव कम करना होता है या जब प्रोग्राम में टेक्स्ट को अपडेट करने की जरूरत होती है।

## कैसे करें:
```Arduino
// उदाहरण कोड
String str = "Hello World";
str.replace("World", "Arduino");
Serial.println(str); // "Hello Arduino" प्रिंट होगा
``` 
इस कोड में, हमने "World" को "Arduino" से बदल दिया और नई स्ट्रिंग का परिणाम "Hello Arduino" प्रिंट किया।

## गहरा घुसाव:
टेक्स्ट को खोज और बदल करने की विधि का इतिहास सीटीयों के लिए संगठनात्मक उपयोग करने के लिए बहुत महत्वपूर्ण होता रहा है। अल्टरनेटिव्स में एक विशेष ढंग से मैन्युअल खोज हो सकती है, लेकिन Arduino का String class के `replace` फंक्शन का उपयोग करके यह प्रक्रिया बहुत ही आसान और तात्कालिक होती है। 

## यदि आप और जानना चाहते हैं:
 
2. [Arduino दस्तावेज़ीकरण: विस्तृत](https://www.arduino.cc/en/Tutorial/Foundations/)

(यदि आपको अन्य Arduino संबंधी स्रोत मिलते हैं, तो आप उन्हें इस सेक्शन में जोड़ सकते हैं।)