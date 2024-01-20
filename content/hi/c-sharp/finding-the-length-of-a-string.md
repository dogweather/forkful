---
title:                "स्ट्रिंग की लंबाई पता करना"
html_title:           "C++: स्ट्रिंग की लंबाई पता करना"
simple_title:         "स्ट्रिंग की लंबाई पता करना"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

स्ट्रिंग की लंबाई पाना इसका अर्थ है, पता करना कि इसमें कितने किरदार होते हैं। इसकी मांग इसलिए होती है क्योंकि इسसे प्रोग्रामर्स को यात्रा को अधिकतम का उपयोग करके स्ट्रिंग पर कार्य करने की अनुमति मिलती है। 

## कैसे:

C# में, आप `Length` property का उपयोग करके स्ट्रिंग की लंबाई का पता लगा सकते हैं। अगर आपकी स्ट्रिंग `"Hello, World!"`, तो उसकी लंबाई 13 है। 

```C#
string myString = "Hello, World!";
int lengthOfString = myString.Length;

Console.WriteLine(lengthOfString); // Prints 13
```

## गहराई में:

विगत के संदर्भ में, उपरोक्त तकनिकी केवल #C# 2.0 से उपलब्ध है। 

`String.Length` property का उपयोग करने का विकल्प माना जा सकता है `StringInfo.LengthInTextElements` जो सुरक्षित और योग्यरूप से Unicode string की लंबाई को मानता है। 

जब  `Length` का उपयोग कोड में किया जाता है, तो .NET runtime आपकी स्ट्रिंग की लंबाई की जांच करता है, और उसकी लंबाई का आन्तःकरणीय निर्धारण करता है। 

## अधिक देखें:

- .NET documentation पर String.Length : https://docs.microsoft.com/dotnet/api/system.string.length
- .NET documentation पर StringInfo.LengthInTextElements : https://docs.microsoft.com/dotnet/api/system.globalization.stringinfo.lenghintextelements