---
title:                "परीक्षण लिखना"
html_title:           "Arduino: परीक्षण लिखना"
simple_title:         "परीक्षण लिखना"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

टेस्ट लिखना मतलब अपने कोड के सही कामकाज की जांच के लिए छोटे प्रोग्राम्स बनाना। प्रोग्रामर्स ये इसलिए करते हैं ताकि सुनिश्चित कर सकें कि उनका कोड बिना किसी गड़बड़ी के सही से काम कर रहा है।

## कैसे करें:

C# में NUnit या MSTest जैसे फ्रेमवर्क्स का उपयोग करके हम टेस्ट्स लिख सकते हैं। यहां एक सरल उदाहरण है:

```c#
using NUnit.Framework;

namespace CalculatorTests
{
    [TestFixture]
    public class CalculatorTest
    {
        [Test]
        public void Add_TwoNumbers_ReturnsSum()
        {
            // Arrange
            int a = 5;
            int b = 10;
            int expectedSum = 15;

            // Act
            int actualSum = Calculator.Add(a, b);

            // Assert
            Assert.AreEqual(expectedSum, actualSum);
        }
    }
}
```

सैम्पल आउटपुट:
```
Test Passed.
```

## गहराई से समझ:

टेस्ट लिखने का चलन 1950 और 1960 के दशक में शुरू हुआ। समय के साथ-साथ टेस्टिंग के तरीके और भी विकसित हुए हैं। TDD (Test-Driven Development) जैसे दृष्टिकोण आज कल ज्यादा प्रचलित हैं। कुछ विकल्पों में xUnit, NUnit, MSTest आदि शामिल हैं जो .NET में टेस्टिंग के लिए उपयोग होते हैं। इन टूल्स के प्रयोग से हमें अपने कोड की कठिनाईयों को समझने और उन्हें बेहतर बनाने में मदद मिलती है।

## और जानें:

- NUnit आधिकारिक वेबसाइट: [NUnit](https://nunit.org/)
- MSTest डॉक्युमेंटेशन: [MSTest](https://docs.microsoft.com/en-us/dotnet/core/testing/unit-testing-with-mstest)
- टेस्ट ड्राइवन डेवलपमेंट क्या है?: [TDD](https://en.wikipedia.org/wiki/Test-driven_development)