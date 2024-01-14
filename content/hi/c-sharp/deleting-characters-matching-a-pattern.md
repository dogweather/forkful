---
title:                "C#: पैटर्न साँचा से अनुरूप अक्षरों को हटाना"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## क्यों
आप अक्सर अपने कोड में किसी भी निश्चित पैटर्न को मेच करने वाले अक्षरों को हटाने की जरूरत महसूस कर सकते हैं। यह अक्सर सभी से काम आता है, चाहे आप अपने स्ट्रिंग को साफ करने के लिए या सुनिश्चित करने के लिए कि क्या आपके कोड में किसी पैटर्न की टिप्पणियां हैं।

## कैसे
```C#
string str = "This is a sample string.";
string removedStr = Regex.Replace(str, "s[a-z]*", ""); // Output: Thi a ample tring.
```
उपरोक्त कोड फ़ंक्शन `str` से `s` शुरू होने वाले किसी भी अक्षरों और उनके बाद के सभी अक्षरों को हटाएगा। यह आकर्षक हो सकता है कि इसमें कोई भी अन्य वर्ण भी हटाए जा सकते हैं, चाहे वे अक्षर हों या वर्णमाला के प्रत्येक अक्षर हों।

## गहराई तक जाएं
यदि आप `Regex` क्लास को गहराई से जानना चाहते हैं, तो आप `RegexOptions` पैरामीटर का उपयोग कर सकते हैं, जहां आप पैटर्न के साथ से कुछ अन्य विकल्प भी चुन सकते हैं। उदाहरण के लिए, जब संभव हो, आप `RegexOptions.IgnoreCase` का उपयोग कर सकते हैं जो अक्षर केस सन्निहित थे या नहीं, उससे प्रभावित होगा।

## देखें भी
[Regex फ़ंक्शन गाइड](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex?view=netcore-3.1) |
[Regex.Replace फ़ंक्शन गाइड](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex.replace?view=netcore-3.1)