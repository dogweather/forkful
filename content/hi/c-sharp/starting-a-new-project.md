---
title:                "नया प्रोजेक्ट शुरू करना"
html_title:           "C: नया प्रोजेक्ट शुरू करना"
simple_title:         "नया प्रोजेक्ट शुरू करना"
programming_language: "C#"
category:             "C#"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/starting-a-new-project.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

नया प्रोजेक्ट शुरू करना मतलब किसी नई समस्या को हल करने हेतु सॉफ्टवेयर निर्माण के उद्देष्य से कोड लिखना शुरू करना। प्रोग्रामर इसे तात्पर्य निर्धारित करने, अच्छे कोड को विकसित करने और बेहतर सॉल्यूशंस प्रदान करने के लिए करते हैं।

## कैसे:

```C#
using System;

namespace NewProjectExample
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("नमस्ते, यह मेरा पहला प्रोजेक्ट है!");
        }
    }
}
```

जब आप इस कोड को चलाते हैं, आपको निम्नलिखित आउटपुट मिलेगा:

```C#
नमस्ते, यह मेरा पहला प्रोजेक्ट है!
```

## गहरा डाइव 

नए project को शुरू करने का कोई इतिहास हिस्टरीकल कांटेक्स्ट नहीं है, परंतु इसकी जड़ें Structured Programming के नीतियों में झूल रही हैं। इस के विकल्पों में कई प्राथमिक आधार और IDEs शामिल हैं, जैसे Visual Studio, JetBrains, आदि। नया प्रोजेक्ट तब शुरू होता है जब हम एक नयी Namespace बनाते हैं और Main मेथड डिफ़ाईन करते हैं। 

## यह भी देखें:

1. [Microsoft: .NET प्रोजेक्ट बनाएं](https://docs.microsoft.com/hi-in/dotnet/core/tools/dotnet-new)
2. [Visual Studio पर प्रोजेक्ट बनाएँ](https://docs.microsoft.com/hi-in/visualstudio/ide/how-to-create-new-projects?view=vs-2022) 
3. [C# प्रोग्रामिंग गाइड](https://docs.microsoft.com/hi-in/dotnet/csharp/)
4. [C# प्रोजेक्ट ट्यूटोरियल](https://www.tutorialsteacher.com/csharp/csharp-programming)