---
date: 2024-01-20 17:55:36.971754-07:00
description: "\u0915\u092E\u093E\u0902\u0921 \u0932\u093E\u0907\u0928 \u0906\u0930\
  \u094D\u0917\u0941\u092E\u0947\u0902\u091F\u094D\u0938 \u092A\u0922\u093C\u0928\u093E\
  \ \u092F\u093E\u0928\u0940 \u092F\u0942\u091C\u093C\u0930 \u0938\u0947, \u092A\u094D\
  \u0930\u094B\u0917\u094D\u0930\u093E\u092E \u091A\u093E\u0932\u0942 \u0939\u094B\
  \u0924\u0947 \u0938\u092E\u092F, \u0907\u0928\u092A\u0941\u091F \u0932\u0947\u0928\
  \u093E\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938\
  \ \u0907\u0938\u0947 \u0907\u0938\u0932\u093F\u090F \u0915\u0930\u0924\u0947 \u0939\
  \u0948\u0902 \u0915\u094D\u092F\u094B\u0902\u0915\u093F \u0907\u0938\u0938\u0947\
  \ \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E \u092B\u094D\u0932\u0947\
  \u0915\u094D\u0938\u093F\u092C\u0932 \u0939\u094B\u0924\u093E \u0939\u0948\u2026"
lastmod: '2024-03-11T00:14:26.270950-06:00'
model: gpt-4-1106-preview
summary: "\u0915\u092E\u093E\u0902\u0921 \u0932\u093E\u0907\u0928 \u0906\u0930\u094D\
  \u0917\u0941\u092E\u0947\u0902\u091F\u094D\u0938 \u092A\u0922\u093C\u0928\u093E\
  \ \u092F\u093E\u0928\u0940 \u092F\u0942\u091C\u093C\u0930 \u0938\u0947, \u092A\u094D\
  \u0930\u094B\u0917\u094D\u0930\u093E\u092E \u091A\u093E\u0932\u0942 \u0939\u094B\
  \u0924\u0947 \u0938\u092E\u092F, \u0907\u0928\u092A\u0941\u091F \u0932\u0947\u0928\
  \u093E\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938\
  \ \u0907\u0938\u0947 \u0907\u0938\u0932\u093F\u090F \u0915\u0930\u0924\u0947 \u0939\
  \u0948\u0902 \u0915\u094D\u092F\u094B\u0902\u0915\u093F \u0907\u0938\u0938\u0947\
  \ \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E \u092B\u094D\u0932\u0947\
  \u0915\u094D\u0938\u093F\u092C\u0932 \u0939\u094B\u0924\u093E \u0939\u0948\u2026"
title: "\u0915\u092E\u093E\u0902\u0921 \u0932\u093E\u0907\u0928 \u0906\u0930\u094D\
  \u0917\u0941\u092E\u0947\u0902\u091F\u094D\u0938 \u092A\u0922\u093C\u0928\u093E"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
कमांड लाइन आर्गुमेंट्स पढ़ना यानी यूज़र से, प्रोग्राम चालू होते समय, इनपुट लेना। प्रोग्रामर्स इसे इसलिए करते हैं क्योंकि इससे प्रोग्राम फ्लेक्सिबल होता है और यूज़र अलग-अलग सिचुएशन के हिसाब से प्रोग्राम को चला सकते हैं।

## How to: (कैसे करें:)
```C#
using System;

class CommandLineDemo
{
    static void Main(string[] args)
    {
        Console.WriteLine("Arguments received:");
        foreach(var arg in args)
        {
            Console.WriteLine(arg);
        }
    }
}
```
सैंपल आउटपुट, अगर आप "dotnet run arg1 arg2 arg3" चलाएं:
```
Arguments received:
arg1
arg2
arg3
```

## Deep Dive (गहन जानकारी)
कमांड लाइन आर्गुमेंट्स UNIX सिस्टम्स पर 1970s से हैं। C# में ये `string[] args` के रूप में `Main` मेथड में पास किए जाते हैं। अल्टरनेटिव में, एन्वायरनमेंट वेरिएबल्स या कॉन्फ़िग्युरेशन फ़ाइल्स का प्रयोग हो सकता है। इसे पढ़ने की डिटेल्स में `System.Environment.GetCommandLineArgs` या PowerShell स्क्रिप्टिंग भी शामिल हो सकती है।

## See Also (और जानकारी के लिए)
- [Microsoft Docs: Command-line arguments (C# Programming Guide)](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/main-and-command-args/)
- [Stack Overflow: How to parse command line arguments](https://stackoverflow.com/questions/491595/best-way-to-parse-command-line-arguments-in-c)
- [CodeProject: Understanding Command Line Arguments in C#](https://www.codeproject.com/Articles/3111/C-NET-Command-Line-Arguments-Parser)
