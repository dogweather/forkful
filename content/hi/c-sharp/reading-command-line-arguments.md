---
title:                "कमांड लाइन आर्गुमेंट्स पढ़ना"
aliases:
- hi/c-sharp/reading-command-line-arguments.md
date:                  2024-01-20T17:55:36.971754-07:00
model:                 gpt-4-1106-preview
simple_title:         "कमांड लाइन आर्गुमेंट्स पढ़ना"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/reading-command-line-arguments.md"
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
