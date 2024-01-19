---
title:                "एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
html_title:           "Kotlin: एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
simple_title:         "एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

स्ट्रिंग को लोअर केस में कनवर्ट करना मतलब उसके सभी अक्षरों को छोटे अक्षरों में बदलना। यह उन मामलों में उपयोगी होता है जबकि हमें यह सुनिश्चित करना होता है कि हमारा कोड केस-सेंसिटिव नहीं हो।

## कैसे करें:

आप निम्नलिखित कोड का उपयोग करके स्ट्रिंग को लोअर केस में कनवर्ट कर सकते हैं:

```C#
string sentence = "Hello World!";
string lowerCaseSentence = sentence.ToLower();
Console.WriteLine(lowerCaseSentence); 
```
सैंपल आउटपुट:

```C#
hello world!
```

## गहराई में:

1. **ऐतिहासिक प्रसंग:** C# भाषा में `ToLower()` फ़ंक्शन का निर्माण .NET Framework के पहले वर्जन के साथ हुआ था। यह समस्या को हल करने का सीधा और सरल तरीका प्रदान करता है।
2. **विकल्प:** `ToLower()` का एक प्रमुख विकल्प `ToLowerInvariant()` है जो कि कल्चर-निरपेक्ष तरीके से एक स्ट्रिंग को लोअर केस में कनवर्ट करता है।
3. **आधार विवरण:** `ToLower()` और `ToLowerInvariant()` फ़ंक्शन उपयोग करते हैं यूनिकोड का डाटा टो फिगर आउट कैसे स्ट्रिंग्स को लोअर केस में कनवर्ट करें।

## देखने के लिए:

1. [Official C# Documentation](https://docs.microsoft.com/en-us/dotnet/csharp/)

2. [String.ToLower Method (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower?view=net-5.0)

3. [String.ToLowerInvariant Method (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolowerinvariant?view=net-5.0)