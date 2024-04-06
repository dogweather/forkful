---
date: 2024-01-20 17:45:21.877638-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902) \u0938\u092C\
  \u0938\u0947 \u092A\u0939\u0932\u0947 \u0909\u092A\u0935\u093E\u0915\u094D\u092F\
  \u093E\u0902\u0936 'Substring' \u0935\u093F\u0927\u093F \u0915\u0947 \u0930\u0942\
  \u092A \u092E\u0947\u0902 .NET Framework \u0915\u0947 \u0906\u0930\u092E\u094D\u092D\
  \ \u092E\u0947\u0902 \u0906\u092F\u093E \u0925\u093E\u0964 \u091C\u092C C# 8.0 \u0906\
  \u092F\u093E, 'Span<T>' \u0928\u094D\u092F\u0942\u0928\u0924\u092E \u092E\u0947\u092E\
  \u094B\u0930\u0940 \u0906\u0935\u0902\u091F\u0928\u2026"
lastmod: '2024-04-05T22:51:07.004693-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902) \u0938\u092C\u0938\u0947\
  \ \u092A\u0939\u0932\u0947 \u0909\u092A\u0935\u093E\u0915\u094D\u092F\u093E\u0902\
  \u0936 'Substring' \u0935\u093F\u0927\u093F \u0915\u0947 \u0930\u0942\u092A \u092E\
  \u0947\u0902 .NET Framework \u0915\u0947 \u0906\u0930\u092E\u094D\u092D \u092E\u0947\
  \u0902 \u0906\u092F\u093E \u0925\u093E\u0964 \u091C\u092C C# 8.0 \u0906\u092F\u093E\
  , 'Span<T>' \u0928\u094D\u092F\u0942\u0928\u0924\u092E \u092E\u0947\u092E\u094B\u0930\
  \u0940 \u0906\u0935\u0902\u091F\u0928 (allocation) \u0915\u0947 \u0938\u093E\u0925\
  \ \u0909\u092A\u0935\u093E\u0915\u094D\u092F\u093E\u0902\u0936 \u0928\u093F\u0915\
  \u093E\u0932\u0928\u0947 \u0915\u093E \u090F\u0915 \u0915\u0941\u0936\u0932 \u0924\
  \u0930\u0940\u0915\u093E \u092C\u0928 \u0917\u092F\u093E\u0964 'Substring' \u0935\
  \u093F\u0927\u093F \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u0940\
  \ \u090F\u0915 \u092A\u094D\u0930\u0924\u093F \u092C\u0928\u093E \u0932\u0947\u0924\
  \u0940 \u0939\u0948, \u091C\u092C\u0915\u093F 'Span<T>' \u092E\u0942\u0932 \u0938\
  \u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u0947 \u092E\u0947\u092E\u094B\
  \u0930\u0940 \u092D\u093E\u0917 \u0915\u093E \u0938\u0940\u0927\u093E \u0938\u0902\
  \u0926\u0930\u094D\u092D \u0926\u0947\u0924\u093E \u0939\u0948\u0964 \u0907\u0938\
  \u0932\u093F\u090F, \u092C\u095C\u0947 \u0921\u0947\u091F\u093E \u092F\u093E \u092A\
  \u0930\u092B\u0949\u0930\u092E\u0947\u0902\u0938-\u0938\u0902\u0935\u0947\u0926\u0928\
  \u0936\u0940\u0932 \u090F\u092A\u094D\u0932\u093F\u0915\u0947\u0936\u0902\u0938\
  \ \u0915\u0947 \u0932\u093F\u090F 'Span<T>' \u091C\u094D\u092F\u093E\u0926\u093E\
  \ \u092C\u0947\u0939\u0924\u0930 \u0939\u094B \u0938\u0915\u0924\u093E \u0939\u0948\
  \u0964."
title: "\u0938\u092C\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0928\
  \u093F\u0915\u093E\u0932\u0928\u093E"
weight: 6
---

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
