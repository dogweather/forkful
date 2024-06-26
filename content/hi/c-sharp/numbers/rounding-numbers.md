---
date: 2024-01-26 03:46:35.292463-07:00
description: "\u0915\u0948\u0938\u0947: \u092F\u0939\u093E\u0901 C# \u092E\u0947\u0902\
  \ \u0938\u0902\u0916\u094D\u092F\u093E\u0913\u0902 \u0915\u094B \u0917\u094B\u0932\
  \ \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0917\u094B\u0932-\u092F\
  \u093E\u0924\u094D\u0930\u093E \u0915\u093E \u091F\u093F\u0915\u091F \u0939\u0948\
  ."
lastmod: '2024-03-13T22:44:52.322986-06:00'
model: gpt-4-0125-preview
summary: "\u092F\u0939\u093E\u0901 C# \u092E\u0947\u0902 \u0938\u0902\u0916\u094D\u092F\
  \u093E\u0913\u0902 \u0915\u094B \u0917\u094B\u0932 \u0915\u0930\u0928\u0947 \u0915\
  \u0947 \u0932\u093F\u090F \u0917\u094B\u0932-\u092F\u093E\u0924\u094D\u0930\u093E\
  \ \u0915\u093E \u091F\u093F\u0915\u091F \u0939\u0948."
title: "\u0938\u0902\u0916\u094D\u092F\u093E\u0913\u0902 \u0915\u094B \u092A\u0942\
  \u0930\u094D\u0923\u093E\u0902\u0915 \u092C\u0928\u093E\u0928\u093E"
weight: 13
---

## कैसे:
यहाँ C# में संख्याओं को गोल करने के लिए गोल-यात्रा का टिकट है:

```csharp
using System;

public class RoundingExamples
{
    public static void Main()
    {
        double originalNumber = 123.4567;

        // नजदीकी पूर्णांक में गोल करें
        double rounded = Math.Round(originalNumber);
        Console.WriteLine(rounded); // आउटपुट: 123

        // दशमलव स्थानों की संख्या निर्दिष्ट करें
        double roundedTwoDecimalPlaces = Math.Round(originalNumber, 2);
        Console.WriteLine(roundedTwoDecimalPlaces); // आउटपुट: 123.46

        // अगले अंक की परवाह किए बिना ऊपर की ओर गोल करें
        double roundedUp = Math.Ceiling(originalNumber);
        Console.WriteLine(roundedUp); // आउटपुट: 124

        // अगले अंक की परवाह किए बिना नीचे की ओर गोल करें
        double roundedDown = Math.Floor(originalNumber);
        Console.WriteLine(roundedDown); // आउटपुट: 123
    }
}
```

## गहराई में जानकारी
पहले के दिनों में, संख्याओं को गोल करना कंप्यूटेशनल लागतों को कम करने के लिए एक सीधी बात थी। हर चक्र मायने रखता था, और संख्याओं को छंटनी करके कीमती समय बचाया जाता था। मॉडर्न C# की ओर तेजी से बढ़ते हुए, यह दोहरी और दशमलव संख्याओं की सटीकता में त्रुटियों और प्रदर्शन की विचित्रताओं के लिए जाने-माने झुकाव का प्रबंधन करने के बारे में है।

`Math.Round`, `Math.Floor`, और `Math.Ceiling` के अलावा, `MidpointRounding` enum हमें मध्य में बैठे बेचारे अंकों की किस्मत तय करने देता है—यह बैंकिंग नियमों और "आधा ऊपर की ओर गोल" के खेल के मैदान के न्याय के बीच का चौराहा है।

कठिन समूहों के लिए, जैसे कि गंभीर गणित या वित्त अनुप्रयोग, हमारे पास `डबल` के बजाय `डेसिमल` है, उच्च सटीकता प्रदान करके गोल करने के नाटक को कम कर रहा है—कम गोल करना, कम समस्याएं।

## यह भी देखें
- [आधिकारिक C# डॉक्स on `Math.Round`](https://docs.microsoft.com/en-us/dotnet/api/system.math.round)
- [Stack Overflow: मुझे कब डबल के बजाय डेसिमल का उपयोग करना चाहिए?](https://stackoverflow.com/questions/1165761/decimal-vs-double-which-one-should-i-use-and-when)
- [फ्लोटिंग-पॉइंट अंकगणित के लिए IEEE मानक (IEEE 754)](https://en.wikipedia.org/wiki/IEEE_754)
