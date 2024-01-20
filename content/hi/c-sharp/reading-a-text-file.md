---
title:                "एक पाठ फ़ाइल पढ़ना"
html_title:           "Bash: एक पाठ फ़ाइल पढ़ना"
simple_title:         "एक पाठ फ़ाइल पढ़ना"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? क्या और क्यों?

"टेक्स्ट फ़ाइल को पढ़ना" का मतलब है कि आपका प्रोग्राम एक फ़ाइल की सामग्री को पढ़ता है और उसे किसी भी तरह के प्रोसेसिंग के लिए प्रयोग करता है। प्रोग्रामर्स इसे करते हैं ताकि वे अपने कोड से बाहर की दुनिया से डेटा के साथ बातचीत कर सकें।

## How to: कैसे करें

एक टेक्स्ट फ़ाइल को पढ़ने के लिए C# में एक मुख्य तरीका है "System.IO.File.ReadAllText()"। चलिए इसे देखते हैं:

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string content = File.ReadAllText("test.txt");
        Console.WriteLine(content);
    }
}
```
इसे चलाने पर, यह आपको "test.txt" फ़ाइल की सामग्री दिखाएगा।

## Deep Dive: गहराई से जानें

तो, "System.IO.File.ReadAllText()" का उपयोग कैसे करता है? इसका जवाब है - यह एक फ़ाइल से एक बार में सभी डेटा को पढ़ता है। यह तभी अच्छा है जब फ़ाइल चोटी हो। बड़ी फ़ाइलों के लिए, इसे आंशिक रूप से पढ़ने के लिए स्ट्रीम ऑब्जेक्ट का उपयोग करना बेहतर होता है।

एक ऐतिहासिक दृष्टिकोण से, C# में टेक्स्ट फ़ाइल पढ़ने वाले कोड का आरंभ .NET फ्रेमवर्क के साथ होता है। इसे तब से अन्य मॉडर्न तकनीकों और पुस्तिकाओं के साथ बड़ा किया गया है, जैसे कि async/await कॉन्सेप्ट्स और `System.IO.File.ReadLines()`.

## See Also: इसके अलावा देखें

संबंधित स्रोतों के लिंक नीचे दिए गए हैं:

1. [ ऑफिशियल C# डॉक्यूमेंटेशन](https://docs.microsoft.com/en-us/dotnet/csharp/)
2. [System.IO.File.ReadAllText()](https://docs.microsoft.com/en-us/dotnet/api/system.io.file.readalltext?view=net-5.0)
3. [System.IO.File.ReadLines()](https://docs.microsoft.com/en-us/dotnet/api/system.io.file.readlines?view=net-5.0)