---
date: 2024-01-20 17:52:10.739230-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) C# \u092E\
  \u0947\u0902 debug messages \u0915\u094B print \u0915\u0930\u0928\u0947 \u0915\u093E\
  \ \u0938\u092C\u0938\u0947 \u0938\u093E\u092E\u093E\u0928\u094D\u092F \u0924\u0930\
  \u0940\u0915\u093E `Console.WriteLine()` \u0939\u0948\u0964 \u092F\u0939\u093E\u0901\
  \ \u090F\u0915 \u0938\u093E\u0927\u093E\u0930\u0923 \u0909\u0926\u093E\u0939\u0930\
  \u0923 \u0939\u0948."
lastmod: '2024-04-05T21:53:54.333987-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) C# \u092E\u0947\u0902\
  \ debug messages \u0915\u094B print \u0915\u0930\u0928\u0947 \u0915\u093E \u0938\
  \u092C\u0938\u0947 \u0938\u093E\u092E\u093E\u0928\u094D\u092F \u0924\u0930\u0940\
  \u0915\u093E `Console.WriteLine()` \u0939\u0948\u0964 \u092F\u0939\u093E\u0901 \u090F\
  \u0915 \u0938\u093E\u0927\u093E\u0930\u0923 \u0909\u0926\u093E\u0939\u0930\u0923\
  \ \u0939\u0948."
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
