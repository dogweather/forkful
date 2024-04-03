---
date: 2024-01-20 17:42:09.629141-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) ."
lastmod: '2024-03-13T22:44:52.304184-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u092A\u0948\u091F\u0930\u094D\u0928 \u0938\u0947 \u092E\u0947\u0932 \u0916\
  \u093E\u0924\u0947 \u0905\u0915\u094D\u0937\u0930\u094B\u0902 \u0915\u094B \u0939\
  \u091F\u093E\u0928\u093E"
weight: 5
---

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
