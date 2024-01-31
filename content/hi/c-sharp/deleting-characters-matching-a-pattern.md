---
title:                "पैटर्न से मेल खाते अक्षरों को हटाना"
date:                  2024-01-20T17:42:09.629141-07:00
model:                 gpt-4-1106-preview
simple_title:         "पैटर्न से मेल खाते अक्षरों को हटाना"

category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
पैटर्न से मेल खाते करैक्टर्स को हटाना मतलब है कि आप किसी स्ट्रिंग से विशेष अक्षरों या टेक्स्ट को निकाल रहे हो। यह तकनीक डेटा को साफ़ करने, अनावश्यक जानकारी को हटाने या फॉर्मेटिंग स्टैंडर्ड्स को लागू करने के लिए उपयोगी होती है।

## How to: (कैसे करें:)
```C#
using System;
using System.Text.RegularExpressions;

class Program {
    static void Main() {
        string sampleText = "Hello, नमस्ते123!";
        string pattern = @"\d";  // इस पैटर्न का मतलब है कोई भी अंक (0-9) मिलेगा, तो हटा दो
        
        // Regex का use करके characters हटाएं
        string cleanedText = Regex.Replace(sampleText, pattern, "");
        Console.WriteLine(cleanedText); // Output: Hello, नमस्ते!
    }
}
```

## Deep Dive (गहराई से विचार):
स्ट्रिंग्स से पैटर्न मैचिंग करके करैक्टर्स हटाने का विचार लंबे समय से है। पुराने जमाने में, प्रोग्रामर्स लूप्स और कंडीशन्स का उपयोग करके इस काम को अंजाम देते थे, लेकिन .NET के आने के बाद से `Regex` (Regular Expression) लाइब्रेरी ने यह काम आसान कर दिया है। `Regex.Replace` मेथड सबसे तेज़ और व्यापक तरीका है। इसके अलावा, LINQ (Language Integrated Query) इस्तेमाल करने का एक और तरीका हो सकता है, लेकिन `Regex` के लिए जटिल पैटर्न्स को हैंडल करना ज्यादा उपयुक्त है।

## See Also (इसे भी देखें):
- [Regular Expressions in .NET](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions)
- [.NET String Class and its methods](https://docs.microsoft.com/en-us/dotnet/api/system.string?view=net-7.0)
- [Regex Class](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex?view=net-7.0)
