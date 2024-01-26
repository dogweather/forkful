---
title:                "सबस्ट्रिंग्स निकालना"
date:                  2024-01-20T17:45:21.877638-07:00
model:                 gpt-4-1106-preview
simple_title:         "सबस्ट्रिंग्स निकालना"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
उपवाक्यांश (substring) निकालना यानी किसी स्ट्रिंग का एक छोटा भाग अलग करना। प्रोग्रामर्स डेटा को प्रोसेस करने, खोजने, या मान्य करने के लिए इसका इस्तेमाल करते हैं।

## How to: (कैसे करें)
```C#
string fullString = "नमस्ते, सीखना सुखद है!";
int startIndex = 8;
int length = 12;

// Substring निकालने का तरीका 1: 'Substring' मेथड का इस्तेमाल
string substring1 = fullString.Substring(startIndex, length);
Console.WriteLine(substring1);  // Output: सीखना सुखद

// तरीका 2: 'Span<T>' और 'Slice' का इस्तेमाल (C# 8.0 से उपलब्ध)
ReadOnlySpan<char> span = fullString.AsSpan();
ReadOnlySpan<char> substring2 = span.Slice(startIndex, length);
Console.WriteLine(substring2.ToString()); // Output: सीखना सुखद
```

## Deep Dive (गहराई से जानकारी)
सबसे पहले उपवाक्यांश 'Substring' विधि के रूप में .NET Framework के आरम्भ में आया था। जब C# 8.0 आया, 'Span<T>' न्यूनतम मेमोरी आवंटन (allocation) के साथ उपवाक्यांश निकालने का एक कुशल तरीका बन गया। 'Substring' विधि स्ट्रिंग की एक प्रति बना लेती है, जबकि 'Span<T>' मूल स्ट्रिंग के मेमोरी भाग का सीधा संदर्भ देता है। इसलिए, बड़े डेटा या परफॉरमेंस-संवेदनशील एप्लिकेशंस के लिए 'Span<T>' ज्यादा बेहतर हो सकता है।

## See Also (और देखें)
- Microsoft's documentation on String.Substring Method: [docs.microsoft.com](https://docs.microsoft.com/en-us/dotnet/api/system.string.substring)
- Microsoft's documentation on Span<T>: [docs.microsoft.com](https://docs.microsoft.com/en-us/dotnet/api/system.span-1) 
- A tutorial on string manipulation in C#: [csharp-tutorials](https://www.csharp-tutorials.com/Data-Types/string-manipulation/)
