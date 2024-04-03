---
date: 2024-01-20 17:52:10.739230-07:00
description: "Debug output \u0915\u093E \u092E\u0924\u0932\u092C \u0939\u094B\u0924\
  \u093E \u0939\u0948 \u0915\u094B\u0921 \u092E\u0947\u0902 \u0938\u0947 \u0938\u0942\
  \u091A\u0928\u093E (information) \u092A\u094D\u0930\u093F\u0902\u091F \u0915\u0930\
  \u0928\u093E \u0924\u093E\u0915\u093F \u0906\u092A \u0938\u092E\u091D \u092A\u093E\
  \u090F\u0902 \u0915\u093F \u0915\u094D\u092F\u093E \u0939\u094B \u0930\u0939\u093E\
  \ \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\
  \u094D\u0938 \u0907\u0938\u0947 \u0907\u0938\u0932\u093F\u090F \u0915\u0930\u0924\
  \u0947 \u0939\u0948\u0902 \u0915\u094D\u092F\u094B\u0902\u0915\u093F \u092F\u0939\
  \ \u092A\u0924\u093E\u2026"
lastmod: '2024-03-13T22:44:52.335827-06:00'
model: gpt-4-1106-preview
summary: "Debug output \u0915\u093E \u092E\u0924\u0932\u092C \u0939\u094B\u0924\u093E\
  \ \u0939\u0948 \u0915\u094B\u0921 \u092E\u0947\u0902 \u0938\u0947 \u0938\u0942\u091A\
  \u0928\u093E (information) \u092A\u094D\u0930\u093F\u0902\u091F \u0915\u0930\u0928\
  \u093E \u0924\u093E\u0915\u093F \u0906\u092A \u0938\u092E\u091D \u092A\u093E\u090F\
  \u0902 \u0915\u093F \u0915\u094D\u092F\u093E \u0939\u094B \u0930\u0939\u093E \u0939\
  \u0948\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938\
  \ \u0907\u0938\u0947 \u0907\u0938\u0932\u093F\u090F \u0915\u0930\u0924\u0947 \u0939\
  \u0948\u0902 \u0915\u094D\u092F\u094B\u0902\u0915\u093F \u092F\u0939 \u092A\u0924\
  \u093E \u0915\u0930\u0928\u0947 \u0915\u093E \u090F\u0915 \u0938\u0930\u0932 \u0924\
  \u0930\u0940\u0915\u093E \u0939\u0948 \u0915\u093F \u0915\u0939\u093E\u0902 \u0914\
  \u0930 \u0915\u094D\u092F\u093E \u0938\u092E\u0938\u094D\u092F\u093E (bug) \u0939\
  \u0948\u0964."
title: "\u0921\u0940\u092C\u0917 \u0906\u0909\u091F\u092A\u0941\u091F \u092A\u094D\
  \u0930\u093F\u0902\u091F \u0915\u0930\u0928\u093E"
weight: 33
---

## How to: (कैसे करें:)
C# में debug messages को print करने का सबसे सामान्य तरीका `Console.WriteLine()` है। यहाँ एक साधारण उदाहरण है:

```C#
using System;

class DebugExample
{
    static void Main()
    {
        Console.WriteLine("Debugging starts here.");
        
        // कुछ कोड जिसे debug करना है
        int result = Sum(5, 3);
        Console.WriteLine($"The result is: {result}");

        // Debug ends
        Console.WriteLine("Debugging ends here.");
    }

    static int Sum(int a, int b)
    {
        // Sum करते वक्त debug information
        Console.WriteLine($"Adding {a} and {b}");
        return a + b;
    }
}
```

Sample Output:
```
Debugging starts here.
Adding 5 and 3
The result is: 8
Debugging ends here.
```

## Deep Dive (गहराई से जानकारी):
Debug output प्रिंट करने का चलन शुरुआती दिनों से ही है जब लोग पंच-कार्ड पर कोड लिखते थे। आज, प्रोग्रामर्स के पास सिस्टम कंसोल से लेकर advanced debugging tools तक बहुत सारे विकल्प होते हैं। C# में आप `Debug.WriteLine()` (सिस्टम डायग्नोस्टिक्स नेमस्पेस का भाग) का भी इस्तेमाल कर सकते हैं, जो कि डेवलपमेंट के दौरान हेल्पफुल होता है लेकिन प्रोडक्शन कोड में इसे नहीं दिखाया जाता।

## See Also (देखें भी):
- Microsoft C# documentation: [https://docs.microsoft.com/dotnet/csharp/](https://docs.microsoft.com/dotnet/csharp/)
- Debugging in Visual Studio: [https://docs.microsoft.com/visualstudio/debugger/](https://docs.microsoft.com/visualstudio/debugger/)
- .NET API for Debug: [https://docs.microsoft.com/dotnet/api/system.diagnostics.debug](https://docs.microsoft.com/dotnet/api/system.diagnostics.debug)
