---
title:    "C#: तस्वीरें लिखना"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## क्यों

टेस्ट कोडिंग आपके प्रोग्राम में सुरक्षा और निर्भरता को सुनिश्चित करता है और बग को पता लगाने में मदद करता है। यह एक अच्छा सॉफ्टवेयर क्वालिटी को गारंटी करने का एक उपाय है।

## कैसे करें

टेस्ट कोडिंग को स्टेप बाइ स्टेप इंस्ट्रक्शन्स के साथ दिखाया गया है, साथ ही उनके साथ सामप्ल कोड जो C# फॉरमैट में है।

```C#
// C# कोड का एक उदाहरण
using System;
using Xunit;

namespace Calculator.Tests
{
    public class CalculatorTests
    {
        [Fact]
        //पूर्वावलोकन को चालू करें
        public void Test_AddTwoNumbers_ReturnsCorrectSum()
        {
            // आरंभ करें
            var calc = new Calculator();

            // कार्रवाई
            var result = calc.Add(2, 5)

            // आश्वासन
            Assert.Equal(7, result);
        }
    }
}
```

इस उदाहरण में, हमने दो संख्याओं को जोड़ने की कार्रवाई को टेस्ट किया है और उससे सही योग को वापस प्रत्याख्यान किया है। इस तरह के सामान्य टेस्ट कोडिंग की सहायता से हम अपने प्रोग्राम में बग की संभावना को कम करते हैं।

## गहराई में

यह ट्यूटोरियल में हमने टेस्ट कोडिंग को संभावित बग और त्रुटियों को पता करने के लिए केस्ट का एक उदाहरण दिखाया है। हमने साधारण संदर्भ में कैसे टेस्ट कोडिंग फायदेमंद हो सकता है। हम इसे अलग-अलग किसी भी भाषा में भी उपयोग कर सकते हैं।

## देखे भी

- [C# का कोड लिखना कैसे सीखें](https://www.codecademy.com/learn/learn-c-sharp)
- [C# सरल उदाहरण्यांचे साहित्य](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/)
- [C# कोड