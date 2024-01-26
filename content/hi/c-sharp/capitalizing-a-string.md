---
title:                "स्ट्रिंग को कैपिटलाइज़ करना"
html_title:           "C: स्ट्रिंग को कैपिटलाइज़ करना"
simple_title:         "स्ट्रिंग को कैपिटलाइज़ करना"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)

स्ट्रिंग को कैपिटलाइज करना मतलब उसे अक्षरों को बड़ा (अपरकेस) बनाना होता है। प्रोग्रामर्स इसे पढ़ने में आसानी, संगठनात्मक मानदंड, या यूजर इंटरफ़ेस में स्थिरता के लिए करते हैं।

## कैसे करें? (How to:)

```C#
using System;

class Program
{
    static void Main()
    {
        string originalText = "नमस्ते! कैसे हैं आप?";
        string capitalizedText = originalText.ToUpper();

        Console.WriteLine(capitalizedText);  // नमस्ते! कैसे हैं आप?
    }
}
```

सैंपल आउटपुट:
```
नमस्ते! कैसे हैं आप?
```

## गहराई से जानकारी (Deep Dive)

ऐतिहासिक तौर पर, कुछ लिपियों में अपरकेस का इस्तेमाल सर्वप्रथम और महत्त्वपूर्ण पत्रों और दस्तावेजों में होता था। C# में `ToUpper()` या `ToUpperInvariant()` का प्रयोग करके आप किसी भी स्ट्रिंग को कैपिटलाइज कर सकते हैं, जो कल्चर-आधारित या कल्चर-निरपेक्ष (इनवेरियंट) कैपिटलाइजेशन प्रदान करते हैं। विकल्प के रूप में, `TextInfo.ToTitleCase()` आपको वाक्यों या टाइटल्स के हर शब्द का पहला अक्षर बड़ा बनाने की सुविधा देता है। हमेशा याद रखें, कुछ भाषाएँ और लिपियाँ अपरकेस-लोअरकेस डिस्टिंक्शन नहीं रखतीं, ऐसे में `ToUpper()` फंक्शन उम्मीद के अनुसार काम नहीं कर सकता।

## संबंधित जानकारियां (See Also)

- Microsoft डॉक्यूमेंटेशन पर [ToUpper](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupper?view=netcore-3.1) 
- Microsoft डॉक्यूमेंटेशन पर [TextInfo.ToTitleCase](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.textinfo.totitlecase?view=netcore-3.1) 
- यूनिकोड पर [Case Mappings](https://www.unicode.org/reports/tr21/tr21-5.html)
