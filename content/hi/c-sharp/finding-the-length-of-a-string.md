---
title:                "स्ट्रिंग की लंबाई का पता लगाना"
html_title:           "C#: स्ट्रिंग की लंबाई का पता लगाना"
simple_title:         "स्ट्रिंग की लंबाई का पता लगाना"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## क्या है और क्यों?:

कोई भी प्रोग्रामिंग भाषा में, एक स्ट्रिंग लंबाई को निर्धारित करने के लिए एक प्रयोग किया जाता है। जब भी हम किसी वस्तु में से तालिका जोड़ने या खोजने की कोशिश करते हैं, तो हमें उस वस्तु की लंबाई को जानने की जरूरत होती है। तो इसी लिए प्रोग्रामर्स इस चीज़ को करते हैं।

## कैसे करें:

```C#
string word = "hello";
int length = word.Length;
Console.WriteLine(length);
//output: 5
```

```C#
string sentence = "This is a sample sentence.";
int length = sentence.Length;
Console.WriteLine(length);
//output: 25
```

## गहराई में जायें:

पहले से ही निर्धारित की गई प्रोग्रामिंग स्ट्रिंग की लंबाई को जानने के लिए, कई विकल्प उपलब्ध हैं। एक विकल्प है कि हम काउंटर चलाएं और हर अक्षर को गिनें। एक और विकल्प है कि हम उस स्ट्रिंग पर एक लूप चला कर उसकी लंबाई के लिए उपयुक्त भाषा में निर्धारण करें। लेकिन अधिकांश समय को बचाने के लिए, हम बस वहीं रहेंगे और एक स्ट्रिंग क्लास का प्रयोग करेंगे। यह काम करने के लिए एक बहुत ही आसान और सरल विकल्प है।

## देखें भी:

मुझे उम्मीद है कि आपको यह लेख पसंद आया होगा और आप इस टॉपिक को समझने में सक्षम होंगे। अगर आपको और जानकारी चाहिए या प्रैक्टिस के लिए और उदाहरण देखना हो तो आप इन लिंक्स को देख सकते हैं:

- [Microsoft डॉक्यूमेंटेशन](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/operators/string-length)
- [और भी जानकारी के लिए ये यूट्यूब वीडियो](https://www.youtube.com/watch?v=mnZsqCFgBL4)