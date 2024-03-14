---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:31.614899-07:00
description: "C# \u092E\u0947\u0902 \u091F\u0947\u0938\u094D\u091F \u0932\u093F\u0916\
  \u0928\u093E \u0906\u092A\u0915\u0947 \u0915\u094B\u0921 \u0915\u0940 \u0915\u093E\
  \u0930\u094D\u092F\u0915\u094D\u0937\u092E\u0924\u093E \u0915\u094B \u092E\u093E\
  \u0928\u094D\u092F \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0938\
  \u094D\u0935\u091A\u093E\u0932\u093F\u0924 \u0938\u094D\u0915\u094D\u0930\u093F\u092A\
  \u094D\u091F\u094D\u0938 \u092C\u0928\u093E\u0928\u0947 \u0915\u0940 \u092A\u094D\
  \u0930\u0915\u094D\u0930\u093F\u092F\u093E \u0939\u094B\u0924\u0940 \u0939\u0948\
  , \u092F\u0939 \u0938\u0941\u0928\u093F\u0936\u094D\u091A\u093F\u0924 \u0915\u0930\
  \u0924\u0940 \u0939\u0948 \u0915\u093F \u0906\u092A\u0915\u093E \u0915\u094B\u0921\
  \ \u0905\u092A\u0947\u0915\u094D\u0937\u093F\u0924\u2026"
lastmod: '2024-03-13T22:44:52.337664-06:00'
model: gpt-4-0125-preview
summary: "C# \u092E\u0947\u0902 \u091F\u0947\u0938\u094D\u091F \u0932\u093F\u0916\u0928\
  \u093E \u0906\u092A\u0915\u0947 \u0915\u094B\u0921 \u0915\u0940 \u0915\u093E\u0930\
  \u094D\u092F\u0915\u094D\u0937\u092E\u0924\u093E \u0915\u094B \u092E\u093E\u0928\
  \u094D\u092F \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0938\u094D\
  \u0935\u091A\u093E\u0932\u093F\u0924 \u0938\u094D\u0915\u094D\u0930\u093F\u092A\u094D\
  \u091F\u094D\u0938 \u092C\u0928\u093E\u0928\u0947 \u0915\u0940 \u092A\u094D\u0930\
  \u0915\u094D\u0930\u093F\u092F\u093E \u0939\u094B\u0924\u0940 \u0939\u0948, \u092F\
  \u0939 \u0938\u0941\u0928\u093F\u0936\u094D\u091A\u093F\u0924 \u0915\u0930\u0924\
  \u0940 \u0939\u0948 \u0915\u093F \u0906\u092A\u0915\u093E \u0915\u094B\u0921 \u0905\
  \u092A\u0947\u0915\u094D\u0937\u093F\u0924\u2026"
title: "\u091F\u0947\u0938\u094D\u091F \u0932\u093F\u0916\u0928\u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?

C# में टेस्ट लिखना आपके कोड की कार्यक्षमता को मान्य करने के लिए स्वचालित स्क्रिप्ट्स बनाने की प्रक्रिया होती है, यह सुनिश्चित करती है कि आपका कोड अपेक्षित तरीके से व्यवहार करे। प्रोग्रामर इसे शुरुआत में ही बग्स को पकड़ने, कोड के रिफैक्टरिंग को सुविधाजनक बनाने, और सुनिश्चित करने के लिए करते हैं कि नए परिवर्तन मौजूदा कार्यों को न तोड़ें, जिससे सॉफ्टवेयर की गुणवत्ता और विश्वसनीयता में वृद्धि होती है।

## कैसे करें:

C# डेवलपर्स मुख्य रूप से NUnit या xUnit फ्रेमवर्क्स का उपयोग टेस्ट लिखने के लिए करते हैं उनकी लचीलापन और व्यापक सुविधा सेट के कारण। यहाँ पर एक साधारण जोड़ फंक्शन की परीक्षण के लिए NUnit का उपयोग करके एक बुनियादी उदाहरण है:

1. **NUnit और NUnit3TestAdapter को NuGet पैकेज मैनेजर या .NET CLI के माध्यम से स्थापित करें**:
```powershell
dotnet add package NUnit
dotnet add package NUnit3TestAdapter
```

2. **C# क्लास लाइब्रेरी प्रोजेक्ट बनाएं**, यदि आपने पहले से नहीं किया है।

3. **परीक्षण के लिए एक साधारण फंक्शन लिखें**। उदाहरण के लिए, `Calculator` नाम की कक्षा में एक जोड़ विधि:
```csharp
public class Calculator
{
    public int Add(int a, int b)
    {
        return a + b;
    }
}
```

4. **NUnit का उपयोग करके एक टेस्ट क्लास लिखें**:
```csharp
using NUnit.Framework;

namespace CalculatorTests
{
    [TestFixture]
    public class CalculatorTests
    {
        [Test]
        public void Add_AddsTwoIntegers_ReturnsCorrectSum()
        {
            // Arrange
            var calculator = new Calculator();
            int expected = 5;

            // Act
            int actual = calculator.Add(2, 3);

            // Assert
            Assert.AreEqual(expected, actual);
        }
    }
}
```

5. **अपने IDE के टेस्ट रनर या .NET CLI का उपयोग करके टेस्ट चलाएं**:
```powershell
dotnet test
```

### उदाहरण आउटपुट:

मान लीजिए आपका टेस्ट पास हो गया है, तो आपको इसी तरह का आउटपुट दिखाई देगा:
```
Test Run Successful.
Total tests: 1
     Passed: 1
 Total time: 1.2345 Seconds
```

### xUnit का उपयोग करते हुए:

यदि आप xUnit को प्राथमिकता देते हैं, तो सेटअप NUnit के समान है। `Calculator` कक्षा के लिए टेस्ट उदाहरण को xUnit का उपयोग करके कैसे पुनः लिखें, यह यहाँ है:

1. **xUnit और xUnit.runner.visualstudio स्थापित करें**:
```powershell
dotnet add package xUnit
dotnet add package xUnit.runner.visualstudio
```

2. **xUnit का उपयोग करके एक टेस्ट क्लास लिखें**:
```csharp
using Xunit;

namespace CalculatorTests
{
    public class CalculatorTests
    {
        [Fact]
        public void Add_AddsTwoIntegers_ReturnsCorrectSum()
        {
            // Arrange
            var calculator = new Calculator();
            int expected = 5;

            // Act
            int actual = calculator.Add(2, 3);

            // Assert
            Assert.Equal(expected, actual);
        }
    }
}
```

3. **.NET CLI या अपने IDE के एकीकृत टेस्ट रनर का उपयोग करके टेस्ट चलाएं**।

NUnit और xUnit दोनों पैरामीटरीकृत परीक्षण, सेटअप/टियरडाउन ऑपरेशंस, और टेस्ट्स को श्रेणियों में व्यवस्थित करने के लिए शक्तिशाली सुविधाएं प्रदान करते हैं, जो कोड गुणवत्ता और कार्यक्षमता को सुनिश्चित करने के लिए C# प्रोग्रामर के उपकरण किट में अनिवार्य उपकरण होते हैं।
