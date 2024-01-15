---
title:                "स्ट्रिंग की लंबाई का पता लगाना"
html_title:           "Java: स्ट्रिंग की लंबाई का पता लगाना"
simple_title:         "स्ट्रिंग की लंबाई का पता लगाना"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## क्यों

कोडिंग करते समय, तस्वीरें या पाठ में बदलाव करते स्ट्रिंग की लंबाई का पता लगाना आम होता है। इसलिए, इस लेख में हम जानेंगे कि Java में स्ट्रिंग की लंबाई कैसे निकाली जाती है। 

## कैसे करें

```
Java String name = "Hindi";
System.out.println("अंक " + name.length() +" परिणाम है।"); 
```

जैसा कि आप देख सकते हैं, स्ट्रिंग की लंबाई को जावा कोड के माध्यम से आसानी से निकाला जा सकता है। `length()` फ़ंक्शन स्ट्रिंग की लंबाई को आपको ट्रैक करने में मदद करता है और परिणाम `5` है, क्योंकि `Hindi` में 5 अक्षर हैं। 

## गहराई के साथ

स्ट्रिंग की लंबाई निकालने के लिए, जावा में `length()` फ़ंक्शन का प्रयोग ज्यादातर कॉमन होता है। यह फ़ंक्शन `String` क्लास से आता है और स्ट्रिंग की लंबाई को ऊंचाई (फ़ंक्शन को बार की मदद से) में देता है। आप इस फ़ंक्शन को `Int` परिणाम के साथ बाहर प्रिंट कर सकते हैं जो स्ट्रिंग की लंबाई को दर्शाता है। 

## नज़र डालें

### और पढ़ें

- [Java नोट्स: जावा में स्ट्रिंग की लंबाई](https://www.javatpoint.com/java-string-length)
- [GeeksforGeeks: जावा में स्ट्रिंग की लंबाई कैसे निकालें](https://www.geeksforgeeks.org/java-string-length-vs-string-length-in-java/)

## देखें

- [Java में स्ट्रिंग प्रोग्रामिंग: बनाओ, तैराओ, बदलो](https://www.learningjournal.guru/article/java-programming/string-programming-in-java/)
- [जावा स्ट्रिंग के 8 उपयोगी फ़ंक्शन्स](https://www.freecodecamp.org/news/java-string-functions-every-developer-should-know/)