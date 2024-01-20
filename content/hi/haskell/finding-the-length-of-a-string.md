---
title:                "स्ट्रिंग की लंबाई पता करना"
html_title:           "C++: स्ट्रिंग की लंबाई पता करना"
simple_title:         "स्ट्रिंग की लंबाई पता करना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

एक वाक्यांश की लंबाई को खोजना मतलब होता है कि वह कितने वर्णों का है। प्रोग्रामर इसे करते हैं क्योंकि ऐसे तत्वों के मात्रा को निर्धारित करना की जरूरत पड़ सकती है। 

## कैसे करें:

Haskell में, आप `length` function का उपयोग करके एक वाक्यांश की
लंबाई का पता लगा सकते हैं:

```Haskell
myString = "नमस्ते दुनिया"
myStringLength = length myString
print myStringLength
```

जब आप ऊपर वाले कोड को चलाएंगे, आपको १० का आउटपुट मिलेगा।

## गहरी जानकारी

**ऐतिहासिक परिप्रेक्ष्य:** Haskell में `length` function का उपयोग करना परंपरागत रूप से किया जाएगा। यह विशेषता भाषा के प्रारंभिक दिनों से ही उपलब्ध है। 

**वैकल्पिक तरीके:** आप कस्टम फ़ंक्शन भी लिख सकते हैं जो हमेशा प्रत्येक करैक्टर को गिने, लेकिन यह `length` फ़ंक्शन से अधिक कोड और समय का उपयोग करेगा।

**विकेन्द्रीकरण विवरण:**  Haskell में `length` function एक लाइब्रेरी फ़ंक्शन है और इसे प्रोग्रामर्स द्वारा फिर से परिभाषित नहीं किया जाता है। इसे केवल इसकी प्रभावी और विश्वसनीयता के कारण प्रयोग किया जाता है।

## अन्य संसाधन

1. [Haskell String Length](https://learnyouahaskell.com/starting-out#an-intro-to-lists): यहां पर आपको `length` function की अधिक जानकारी मिलेगी।