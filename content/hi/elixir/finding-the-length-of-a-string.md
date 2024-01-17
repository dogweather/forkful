---
title:                "स्ट्रिंग की लंबाई ढूंढना"
html_title:           "Elixir: स्ट्रिंग की लंबाई ढूंढना"
simple_title:         "स्ट्रिंग की लंबाई ढूंढना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
आज के एक बड़े हिस्से में, हमारे पास बहुत सारे डिजिटल डेटा है जिसमें शामिल स्ट्रिंग्स हैं। ये स्ट्रिंग्स हमें दिए गए जानकारी को स्टोर और मैनिपुलेट करने में मदद करती हैं। प्रोग्रामर्स कभी-कभी इन स्ट्रिंग्स की लंबाई को जानने के लिए उनका प्रयोग करते हैं।

## कैसे करें:
Elixir में, हम फ़ंक्शन `String.length()` का उपयोग करके स्ट्रिंग की लंबाई को जान सकते हैं। नीचे दिए गए उदाहरण में, हम स्ट्रिंग "Hello World" की लंबाई को प्रिंट करते हैं।

```
Elixir कोड:
String.length("Hello World")

आउटपुट:
11
```

## गहराई में जाएं:
Elixir में स्ट्रिंग की लंबाई को जानने के लिए, अंतर्निहित रूप से `length/1` फ़ंक्शन का उपयोग किया जाता है। यह फ़ंक्शन स्ट्रिंग मॉड्यूल में परिभाषित है और एक एटम या स्ट्रिंग आर्ग्यूमेंट लेता है। अन्य भाषाओं में भी स्ट्रिंग की लंबाई को जानने के लिए `length()` फ़ंक्शन का उपयोग किया जाता है।

## अन्य संबंधित स्रोत देखें:
- [Elixir स्ट्रिंग मॉड्यूल](https://hexdocs.pm/elixir/String.html)
- [Stack Overflow पोस्ट: How to get the length of a string in Elixir](https://stackoverflow.com/questions/43784741/how-to-get-the-length-of-a-string-in-elixir)