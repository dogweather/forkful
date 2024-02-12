---
title:                "स्ट्रिंग को कैपिटलाइज करना"
aliases: - /hi/c-sharp/capitalizing-a-string.md
date:                  2024-02-03T19:06:20.769508-07:00
model:                 gpt-4-0125-preview
simple_title:         "स्ट्रिंग को कैपिटलाइज करना"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
C# में एक स्ट्रिंग को कैपिटलाइज़ करने का मतलब है पहले अक्षर को अगर वह पहले से अपरकेस में नहीं है तो उसे अपरकेस में बदलना। यह परिवर्तन आउटपुट्स को फॉर्मेट करने, कोडिंग मानकों को लागू करने, या उपयोगकर्ता इंटरफ़ेस टेक्स्ट को अधिक पठनीय बनाने के लिए महत्वपूर्ण हो सकता है।

## कैसे:
C# बिल्ट-इन मेथड्स का उपयोग करके स्ट्रिंग्स को कैपिटलाइज़ करने का एक सीधा तरीका प्रदान करता है। इसे हासिल करने का सबसे सरल तरीका इन मेथड्स के साथ सीधे स्ट्रिंग में संशोधन करना है। अधिक जटिल या विशिष्ट कैपिटलाइजेशन नियमों के लिए (जैसे, प्रत्येक शब्द को कैपिटलाइज़ करना), अतिरिक्त लाइब्रेरीज़ या मैनुअल मेथड्स आवश्यक हो सकते हैं। नीचे C# में विभिन्न तरीकों से स्ट्रिंग को कैपिटलाइज कैसे करें इसके उदाहरण दिए गए हैं।

### बुनियादी कैपिटलाइजेशन:
एक शब्द या वाक्य के पहले अक्षर को कैपिटलाइज करने के लिए:

```csharp
string originalString = "hello world";
string capitalizedString = char.ToUpper(originalString[0]) + originalString.Substring(1);
Console.WriteLine(capitalizedString); // आउटपुट: "Hello world"
```

### प्रत्येक शब्द को कैपिटलाइज करना:
स्ट्रिंग में प्रत्येक शब्द के पहले अक्षर को कैपिटलाइज़ करने के लिए, आप `System.Globalization` नेमस्पेस में पाए जाने वाले `TextInfo.ToTitleCase` मेथड का उपयोग कर सकते हैं:

```csharp
using System;
using System.Globalization;

string originalString = "hello world";
TextInfo textInfo = CultureInfo.CurrentCulture.TextInfo;
string capitalizedString = textInfo.ToTitleCase(originalString);
Console.WriteLine(capitalizedString); // आउटपुट: "Hello World"
```

ध्यान दें: `ToTitleCase` शेष अक्षरों को लोअर केस में नहीं बदलता; यह केवल प्रत्येक शब्द के पहले अक्षर को अपरकेस में बदलता है। साथ ही, टाइटल केस नियमों में कुछ शब्द (जैसे "and", "or", "of") सांस्कृतिक सेटिंग्स के आधार पर कैपिटलाइज़ नहीं हो सकते हैं।

### पुन: प्रयोज्यता के लिए एक्सटेंशन मेथड्स का उपयोग:
आप `string` क्लास के लिए एक एक्सटेंशन मेथड बनाकर कैपिटलाइजेशन प्रक्रिया को सरल बना सकते हैं, अपने कोड को सा�र्य और अधिक पुन: प्रयोज्य बनाते हैं। ऐसा मेथड बनाने और उपयोग करने का तरीका यहाँ दिया गया है:

```csharp
using System;

public static class StringExtensions
{
    public static string Capitalize(this string input)
    {
        if (string.IsNullOrEmpty(input))
        {
            return input;
        }
        return char.ToUpper(input[0]) + input.Substring(1);
    }
}

class Program
{
    static void Main(string[] args)
    {
        string originalString = "hello world";
        string capitalizedString = originalString.Capitalize();
        Console.WriteLine(capitalizedString); // आउटपुट: "Hello world"
    }
}
```

यह एक्सटेंशन मेथड `Capitalize` नामस्थान के भीतर किसी भी स्ट्रिंग ऑब्जेक्ट पर कॉल किया जा सकता है, C# में स्ट्रिंग मैनिपुलेशन के लिए एक और सहज और वस्तु-उन्मुख दृष्टिकोण प्रदान करता है। 

### थर्ड-पार्टी लाइब्रेरीज:
जबकि C# की स्टैंडर्ड लाइब्रेरी स्ट्रिंग कैपिटलाइजेशन के अधिकांश आवश्यकताओं को पूरा करती है, कुछ विशेषज्ञ कार्यों को थर्ड-पार्टी लाइब्रेरीज, जैसे कि Humanizer से लाभ हो सकता है। हालांकि, स्ट्रिंग्स को सरलता से कैपिटलाइज करने या स्ट्रिंग में प्रत्येक शब्द को कैपिटलाइज करने के कार्य के लिए, मानक C# मेथड्स पर्याप्त और कुशल होते हैं, बाहरी निर्भरताओं की आवश्यकता को नकारते हैं।
