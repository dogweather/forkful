---
title:                "कम्प्यूटर प्रोग्रामिंग में यादृच्छिक संख्याओं का उत्पादन"
html_title:           "C#: कम्प्यूटर प्रोग्रामिंग में यादृच्छिक संख्याओं का उत्पादन"
simple_title:         "कम्प्यूटर प्रोग्रामिंग में यादृच्छिक संख्याओं का उत्पादन"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

# क्यों

क्या आप कभी ऐसी स्थिति में थे जब आपको कुछ रैंडम नंबर्स की आवश्यकता हुई हो? हायटेक सुरक्षा, डाटा विज्ञान, या विश्वसनीयता के लिए? रैंडम नंबर्स को जनरेट करना एक उपभोक्ता के लिए कुछ विशेष उपयोगता हो सकती है। इस लेख में हम जानेंगे कि आप C# में रैंडम नंबर्स कैसे जनरेट कर सकते हैं और यह उन विषयों को गहराई से समझेंगे जो इस प्रक्रिया से संबंधित हैं।

अब हम सीखेंगे कि C# में रैंडम नंबर्स कैसे जनरेट करते हैं। सरल उदाहरणों के साथ "```C#...```" कोड ब्लॉक।

# कैसे करें

उदाहरण 1:
```C#
using System;

namespace RandomNumber
{
    class Program
    {
        static void Main(string[] args)
        {
            Random rand = new Random(); // Random क्लास के एक नया ऑब्जेक्ट बनाएं
            int randomNumber = rand.Next(10); // 1 से 10 तक के बीच एक रैंडम नंबर जनरेट करें
            Console.WriteLine(randomNumber); // रैंडम नंबर को प्रिंट करें
        }
    }
}
```
उपरोक्त कोड, 1 से 10 तक के बीच से एक रैंडम नंबर को बंटाता है और इसे प्रिंट करता है। अगर आप चाहते हैं कि रैंडम नंबर एक निश्चित सीमा के बीच हो तो आप `Next()` की जगह `Next(min, max)` का उपयोग कर सकते हैं।

उदाहरण 2:
```C#
using System;

namespace RandomNumber
{
    class Program
    {
        static void Main(string[] args)
        {
            Random rand = new Random(); // Random क्लास के एक नया ऑब्जेक्ट बनाएं
            double randomNumber = rand.NextDouble(); // 0 से 1 तक के बीच एक रैंडम नंबर जनरेट करें
            Console.WriteLine(randomNumber); // रैंडम नंबर को प्रिंट करें
        }
    }
}
```

उपरोक