---
title:    "Arduino: रेगुलर एक्सप्रेशन का उपयोग करना"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## क्यों

आर्डुइनो प्रोग्रामिंग में नियमित अभिव्यक्तियों (regular expressions) का उपयोग करने के पीछे एक बहुत ही सरल वजह है - वे स्ट्रिंग (strings) में सामान्य शब्दों को जोड़ने या छोड़ने को सुगम बनाते हैं।

## कैसे

```Arduino
// उदाहरण 1:  "hello" स्ट्रिंग में "lo" के आधार पर खोज करें 
if (Regex.find("hello", "lo")) {
  Serial.println("मैच मिल गया!");
} else {
  Serial.println("मैच नहीं मिला!");
}

// Output:
// मैच मिल गया!
```

```Arduino
// उदाहरण 2: "123abc456" स्ट्रिंग से संबंधित संख्यात्मक सामग्री में मिली '"abc"' को छोड़ें
String result = Regex.remove("123abc456", "abc");
Serial.println(result);

// Output:
// 123456
```

## गहराई में जाएं

नियमित अभिव्यक्तियों की और अधिक गहराई में जाने के लिए, आप नियमित अभिव्यक्तियों का अध्ययन कर सकते हैं जो Arduino बोर्ड में उपलब्ध हैं। इनका उपयोग स्ट्रिंग प्रोसेसिंग से सम्बंधित समस्याओं को हल करने में काफी सहायक हो सकता है।

## देखें भी

- [Arduino के लिए Regex की पोस्ट](https://www.arduino.cc/reference/en/language/functions/communication/serial/begin/)
- [Regexp का Wikipedia पृष्ठ](https://en.wikipedia.org/wiki/Regular_expression)