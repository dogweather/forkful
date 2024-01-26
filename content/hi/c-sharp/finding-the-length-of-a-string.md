---
title:                "स्ट्रिंग की लंबाई ज्ञात करना"
date:                  2024-01-20T17:47:29.423576-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग की लंबाई ज्ञात करना"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
String की length जानना मतलब होता है character की संख्या को मापना। यह इसलिए जरूरी है क्योंकि programmers को अक्सर data या input को validate और process करने के लिए इसका सही आकार जानना अनिवार्य होता है।

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
