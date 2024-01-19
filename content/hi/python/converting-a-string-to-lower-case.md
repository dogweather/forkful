---
title:                "एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
html_title:           "Kotlin: एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
simple_title:         "एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# **Python में String को Lower Case में कैसे कन्वर्ट करें?** 

## **क्या और क्यों?**

string को lower case में कन्वर्ट करना मतलब है कि आप सभी अक्षरों को छोटे अक्षरों में बदल रहे हैं। कुछ मामलों में, यह समानता की जांच करने में सहायता करता है, जैसे कि यदि आप किसी user input से बड़े/छोटे अक्षरों को हटाना चाहते हैं। 

## **कैसे करें:**

Python में, हम `.lower()` मेथड का उपयोग करके किसी string को lower case में कन्वर्ट कर सकते हैं। यहां एक उदाहरण है:

```Python 
str = "HELLO, दुनिया!"
print(str.lower())
```

आउटपुट: 

```Python 
"hello, दुनिया!"
```

## **गहराई में जानकारी:**

`.lower()` मेथड Python के स्टैंडर्ड लाइब्रेरी का हिस्सा है और यह Unicode संगत है, इसका मतलब है कि इसका उपयोग विभिन्न भाषाओं और script के साथ किया जा सकता है। 

लेने-देने को ध्यान में रखते हुए, `.casefold()` को भी उपयोग किया जा सकता है, जो भी एक Python मेथड है और ज्यादा aggressive होता है। लेकिन, `.casefold()` का उपयोग सामान्यतः उन मामलों में किया जाता है जब string Unicode characters को शामिल करता है। 

इसका सही उपयोग सबसे बड़ी चुनौती हो सकती है, क्योंकि यह भाषा-स्पेसिफिक मामलों को नही संभाला जाता है।

## **इसके अलावा देखें:**

1. [Python String lower() मेथड विवरण](https://www.tutorialspoint.com/python/string_lower.htm)
2. [casefold() Vs lower() in Python](https://www.geeksforgeeks.org/casefold-in-python/)