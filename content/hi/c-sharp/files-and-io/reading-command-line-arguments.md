---
date: 2024-01-20 17:55:36.971754-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) ."
lastmod: '2024-03-13T22:44:52.358725-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u0915\u092E\u093E\u0902\u0921 \u0932\u093E\u0907\u0928 \u0906\u0930\u094D\
  \u0917\u0941\u092E\u0947\u0902\u091F\u094D\u0938 \u092A\u0922\u093C\u0928\u093E"
weight: 23
---

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
