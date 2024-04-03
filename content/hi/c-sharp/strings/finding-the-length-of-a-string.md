---
date: 2024-01-20 17:47:29.423576-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) ."
lastmod: '2024-03-13T22:44:52.316139-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u0940 \u0932\u0902\
  \u092C\u093E\u0908 \u091C\u094D\u091E\u093E\u0924 \u0915\u0930\u0928\u093E"
weight: 7
---

## How to: (कैसे करें:)
```C#
using System;

class Program
{
    static void Main()
    {
        string myString = "नमस्ते, दुनिया!";
        int length = myString.Length;
        
        Console.WriteLine("String length: " + length); // Output: String length: 14
    }
}
```
उपरोक्त कोड दिखाता है कि कैसे हम `Length` property का इस्तेमाल करके string की लंबाई निकाल सकते हैं।

## Deep Dive (गहराई से जानकारी)
C# में string की length जानने की क्षमता शुरू से मौजूद है। एक string की length property एक integer value लौटाती है जो characters की संख्या दिखाती है।

वैकल्पिक रूपों में, `LINQ` का उपयोग करके भी string length निकाली जा सकती है:
```C#
using System;
using System.Linq;

class Program
{
    static void Main()
    {
        string myString = "नमस्ते, दुनिया!";
        int length = myString.Count();
        
        Console.WriteLine("String length using LINQ: " + length); // Output: String length using LINQ: 14
    }
}
```
ध्यान रखें कि `Length` सबसे तेज और आसान तरीका है, `LINQ` का `Count()` तब इस्तेमाल किया जाता है जब और जटिल query करनी हो।

Unicode characters जैसे कि इमोजी या विशेष characters जो सरोगेट pairs के रूप में encode किए जाते हैं, `Length` का परिणाम भ्रामक कर सकते हैं क्योंकि वे एक से अधिक UTF-16 units का प्रतिनिधित्व करते हैं।

## See Also (और देखें)
- Microsoft's official documentation on Strings in C#: [https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/)
- A discussion on how string indexing works in C#: [https://stackoverflow.com/questions/394616/running-a-c-sharp-exe-without-visual-studio](https://stackoverflow.com/questions/394616/running-a-c-sharp-exe-without-visual-studio)
- For a more thorough understanding of encoding and string operations: [https://docs.microsoft.com/en-us/dotnet/standard/base-types/character-encoding](https://docs.microsoft.com/en-us/dotnet/standard/base-types/character-encoding)
