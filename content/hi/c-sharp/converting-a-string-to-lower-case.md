---
title:    "C#: स्ट्रिंग को लोअर केस में रूपांतरण करना"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्यों

ऑनलाइन माध्यम के साथ-साथ आप किसी भी प्रोग्रामिंग भाषा में काम करते हैं, तो स्ट्रिंग को लोअर केस में बदलने की आवश्यकता अक्सर आती है। अगर आपको कंप्यूटर द्वारा दिए गए इंस्ट्रक्शन्स (Intructions) को समझना होता है, तो स्ट्रिंग को लोअर केस में बदलना आपको उसे अधिक आसान बना सकता है।

## कैसे

```c#
string originalString = "HINDI BLOG POST";
string lowerCaseString = originalString.ToLower();

Console.WriteLine(lowerCaseString);
```

आउटपुट:

```
hindi blog post
```

## गहराई तक में जानिए

स्ट्रिंग को लोअर केस में बदलना अक्सर अस्पष्ट जानकारी हो सकता है। लेकिन स्ट्रिंग हमेशा कैसे हैंडल होता है, इसको समझना आपको प्रोग्रामिंग में अधिक निपुणता और निश्चितता का मार्ग प्रदान कर सकता है। इस आर्टिकल में, हम देखेंगे कि स्ट्रिंग को लोअर केस में बदलने के लिए C# में कैसे कोड शुरू करें और इस चीज़ का आप अपने कोड में कैसे इस्तेमाल कर सकते हैं।

## देखें भी

- [C# की स्ट्रिंग को अलग करना](https://docs.microsoft.com/en-us/dotnet/api/system.string.tostring?view=netcore-3.1)
- [C# में कैसे लूप को उपयोग करें](https://www.c-sharpcorner.com/article/using-loops-in-C-Sharp/)
- [C# में ग्राफिक्स और ग्राफिकल क्षेत्र](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/graphics/)