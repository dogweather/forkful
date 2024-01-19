---
title:                "स्ट्रिंग की लंबाई पता करना"
html_title:           "C++: स्ट्रिंग की लंबाई पता करना"
simple_title:         "स्ट्रिंग की लंबाई पता करना"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
'String की लंबाई को खोजना' शब्दों की संख्या या किसी शब्द संग्रह की लंबाई जानने की प्रक्रिया है। इसकी सहायता से, प्रोग्रामर कोड की कार्यक्षमता को अनुकूलित कर सकते हैं, जैसे कि इनपुट आंकड़ों की जांच करना।

## कैसे करें:
Java में, हम `length()` method का उपयोग करके String की लंबाई मिटाते हैं। यहां एक उदाहरण है:

```Java
public class Main {
    public static void main(String[] args) {
        String str = "नमस्ते दुनिया";
        int len = str.length();
        System.out.println("String की लंबाई: " + len);
    }
}
```

यहां, सबसे पहले हमने एक String बनाया "नमस्ते दुनिया", फिर हमने `length()` method का उपयोग किया, जिसने शब्दों की संख्या को दिया। जवा कन्सोल पर आउटपुट यह होगा: 

```Java
"String की लंबाई: 12"
```

## गहरी जांच:
जबसे Java का पहला संस्करण 1996 में आया था, `length()` method वहां था। यह विशेषता बहुत महत्वपूर्ण है क्योंकि यह एक ऐसी जरूरत को पूरा करता है जो प्रोग्रामर्स को अक्सर आती है। 

`length()` के विकल्प के रूप में, आप `toCharArray()` function का उपयोग कर सकते हैं, यह शब्द संग्रह को अक्षरों की सूची में परिवर्तित करेगा और फिर आपको उसकी लंबाई मिलेगी। हालांकि, यह तरीका `length()` function से अधिक संसाधन उपयोगी हो सकता है। 

`length()` method के आंतरिक विवरण के अनुसार, यह Java के `java.lang.String` वर्ग का एक member function है, जिसे संग्रह की लंबाई जानने के लिए call किया जा सकता है।

## और भी देखें:
- [Java String वर्ग (जवा ट्यूटोरियल) (अंग्रेजी)](https://www.w3schools.com/java/java_strings.asp)
- [String length() Method और उसका इस्तेमाल कैसे करें (अंग्रेजी)](https://www.javatpoint.com/java-string-length)