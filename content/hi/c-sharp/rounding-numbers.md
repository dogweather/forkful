---
title:                "संख्याओं को पूर्णांक बनाना"
date:                  2024-01-26T03:46:35.292463-07:00
model:                 gpt-4-0125-preview
simple_title:         "संख्याओं को पूर्णांक बनाना"

category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/rounding-numbers.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
संख्याओं को गोल करने का मतलब है उन्हें निकटतम निर्दिष्ट स्थान मूल्य में समायोजित करना—सोचिए उन्हें एक सरल रूप में बांधना। प्रोग्रामर सटीकता को नियंत्रित करने, प्रदर्शन को बढ़ाने, या जब उपयोगकर्ता-अनुकूल परिणाम दिखाने के लिए - जैसे कीमतें जिन्हें तीन दशमलव स्थानों की आवश्यकता नहीं है - संख्याओं को गोल करते हैं।

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
