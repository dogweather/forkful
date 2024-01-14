---
title:    "Arduino: स्ट्रिंग की लंबाई का पता लगाना"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## क्यों

क्या आपको यह पता है कि आर्डुइनो में एक स्ट्रिंग की लंबाई कैसे ढूँढी जा सकती है? अगर नहीं, तो आज हम इस विषय पर बात करेंगे। यह एक उपयोगी कौशल है जो आपको अपने आर्डुइनो प्रोजेक्ट में मदद कर सकता है।

## कैसे करें

आर्डुइनो में स्ट्रिंग की लंबाई को खोजने के लिए, हम `strlen()` फंक्शन का इस्तेमाल कर सकते हैं। नीचे दिए गए कोड ब्लॉक में हम इसे लागू करेंगे। 

```Arduino
// स्ट्रिंग बनाएं
char str[] = "हैलो दुनिया";

// स्ट्रिंग की लंबाई ढूँढें
int len = strlen(str);

// उत्पादन प्रिंट करें
Serial.println(len);

// प्रिंट: 11
```

## दीवानगी

`strlen()` फंक्शन एक सरल और उपयोगी तरीका है जो आर्डुइनो में स्ट्रिंग की लंबाई को ढूँढने के लिए उपलब्ध है। इसका उपयोग आपके प्रोजेक्ट में स्ट्रिंग को हैंडल करने को आसान बना सकता है। यह फंक्शन आपको विभिन्न भाषाओं में भी इस्तेमाल करने की अनुमति देता है।

## देखें भी

- [आर्डुइनो स्ट्रिंग फंक्शन्स](https://www.arduino.cc/reference/en/language/functions/communication/stream/)
- [स्ट्रिंग मानिपुलेशन का संबंधित चित्र](https://arduino-info.wikispaces.com/YazStringDiagram2)