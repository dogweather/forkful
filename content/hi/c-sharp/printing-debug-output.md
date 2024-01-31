---
title:                "डीबग आउटपुट प्रिंट करना"
date:                  2024-01-20T17:52:10.739230-07:00
model:                 gpt-4-1106-preview
simple_title:         "डीबग आउटपुट प्रिंट करना"

category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
Debug output का मतलब होता है कोड में से सूचना (information) प्रिंट करना ताकि आप समझ पाएं कि क्या हो रहा है। प्रोग्रामर्स इसे इसलिए करते हैं क्योंकि यह पता करने का एक सरल तरीका है कि कहां और क्या समस्या (bug) है।

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
