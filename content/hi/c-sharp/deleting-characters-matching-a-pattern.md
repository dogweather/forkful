---
title:                "पैटर्न से मिलते जुलते वर्णों को हटाना"
html_title:           "Elixir: पैटर्न से मिलते जुलते वर्णों को हटाना"
simple_title:         "पैटर्न से मिलते जुलते वर्णों को हटाना"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# पैटर्न से मेल खाने वाले वर्णों को हटाना (Deleting Characters Matching a Pattern)

## क्या और क्यों? (What & Why?)

वर्णों को पैटर्न से मेल खाने के हिसाब से हटाना मतलब होता है की आप उस वर्ण को हटा देते हैं, जो निर्दिष्ट पैटर्न से मेल खाता है। प्रोग्रामर्स इसे इसलिए करते हैं क्योंकि ऐसा करने से कोड के भागों को साफ करने और पाठ डेटा को संशोधित करने में मदद मिलती है।

## कैसे: (How to:)

हम `Regex.Replace` का उपयोग करके स्ट्रिंग से चरित्र हटा सकते हैं। निम्नलिखित C# कोड संग्रहण में, हम ["१", "२", "३"] पैटर्न से मेल खाते सभी वर्णों को हटा रहे हैं।

```C#
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        var inputStr = "मेरी उम्र ३० वर्ष है।";
        var pattern = "[१२३]";
        var replacedStr = Regex.Replace(inputStr, pattern, string.Empty);
        Console.WriteLine(replacedStr);  // Outputs: मेरी उम्र ० वर्ष है।
    }
}
```

## गहरा विवेचन (Deep Dive):

1. **ऐतिहासिक संदर्भ (Historical context):** वर्णों को हटाना या बदलना पाठ संशोधन का मूल तत्व है, जो विभिन्न कार्यक्रमों में वर्णों, शब्दों या वाक्यांशों को बदलने की अनुमति देता है। 
   
2. **वैकल्पिक (Alternatives):** `Regex.Replace` के अलावा, `StringBuilder` और लूप्स का भी इस्तेमाल किया जा सकता है जबकि यह एक अधिक लगवग समाधान हो सकता है।
   
3. **कार्यान्वयन विवरण (Implementation details):** `Regex.Replace` विधि स्ट्रिंग को पैटर्न के साथ मिलान ढूंढ़ती है, और हर मिलान हटा देती है जब आवश्यक हो। यह मीटिंग पतटर के साथ-साथ जटिल पैटर्न्स के साथ भी काम करता है।

## और भी देखें (See Also):

1. [Microsoft's Guide to .NET Regular Expressions](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions) - रेगुलर एक्सप्रेशन के अधिक उपयोगों के लिए।
2. [Microsoft's String Manipulation Guide](https://docs.microsoft.com/en-us/dotnet/csharp/how-to/modify-string-contents) - अन्य स्ट्रिंग मणिपुलेशन की तकनीकों के लिए।