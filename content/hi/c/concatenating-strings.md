---
title:                "स्ट्रिंग्स को आपस में जोड़ना"
html_title:           "C: स्ट्रिंग्स को आपस में जोड़ना"
simple_title:         "स्ट्रिंग्स को आपस में जोड़ना"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## क्या & क्यूँ?
कंकेटनेटिंग स्ट्रिंग एक ऐसी प्रक्रिया है जिसमें दो या अधिक स्ट्रिंग्स को संयोजित करके एक ही स्ट्रिंग बनाई जाती है। प्रोग्रामर्स इसे अपने कोड को कंप्यूटर को ग्राहकों को सुविधाजनक बनाने के लिए करते हैं।

## कैसे करें:
स्ट्रिंग को कंकेटनेट करने के लिए, 'strcat' फ़ंक्शन का उपयोग किया जाता है। इसका सिंटैक्स निम्नलिखित है:
```C
strcat(destination, source);
```
यदि हमें दो स्ट्रिंग्स "Hello" और "World" को कंकेटनेट करना है, तो निम्नलिखित कोड का उपयोग कर सकते हैं:
```C
char destination[12]="Hello";
char source[6]="World";
strcat(destination, source);
printf("Concatenated string: %s", destination);
```
आउटपुट:
```C
Concatenated string: HelloWorld
```

## गहराई में जाएं:
यह प्रक्रिया पहले से ही है। और इसका मुख्य उपयोग स्ट्रिंग्स को एक साथ मिलाने के लिए होता है। अन्य विकल्पों के रूप में, 'strncat' फ़ंक्शन भी है, जो एक निश्चित संख्या के अक्षरों तक ही स्ट्रिंगों को कंकेटनेट करता है। इसका प्रयोग भाषा फ़ंक्शन्स को लम्बे स्ट्रिंग्स को तोड़ने से बचाने में किया जाता है।

'Concatenation' शब्द लैटिन शब्द "concatenare" से आया है, जिसका अर्थ है "एक साथ जोड़ना"। कंकेटनेट करने वाले टेक्निक स्ट्रिंग्स हो प्रोग्रामिंग मैं भी उपयोग किया गया है।

## देखें भी:
अगर आपको 'strcat' और अन्य भाषा फ़ंक्शनों के बारे में और जानकारी चाहिए तो आप निम्नलिखित स्रोतों का उपयोग कर सकते हैं:
- [C भाषा ट्यूटोरियल](https://www.programiz.com/c-programming/string-handling-functions)
- [Concatenation के बारे में Wikipedia की जानकारी](https://en.wikipedia.org/wiki/Concatenation)