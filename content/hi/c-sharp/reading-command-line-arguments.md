---
title:                "कमांड लाइन तर्कों को पढ़ना"
html_title:           "Kotlin: कमांड लाइन तर्कों को पढ़ना"
simple_title:         "कमांड लाइन तर्कों को पढ़ना"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Reading Command Line Arguments in C#

## क्या और क्यों?
Command Line Arguments वह values होती हैं जो किसी application को command-line से पास की जाती हैं। Programmers इसका use application को अधिक flexible और customizable बनाने के लिए करते हैं।

## कैसे करें:
Aao देखते हैं कैसे C# में command line arguments read की जा सकती हैं।

```C#
static void Main(string[] args)
{
    for (int i = 0; i < args.Length; i++)
    {
        Console.WriteLine("Arg[" + i + "]: " + args[i]);
    }
}
```
जब आप ऊपर के code को किसी arguments के साथ run करोगे, तो output कुछ ऐसा दिखाई देगा:
```C#
Arg[0]: FirstArgument
Arg[1]: SecondArgument
...
```
## गहरी बातचीत:
1. Historical context: कुछ पुरानी languages, जैसे की C और C++, में command line arguments को process करने के लिए विशेष methods या functions की जरूरत होती थी। C# में यह functionality built-in है।
2. Alternatives: अगर आपको arguments का advanced uses करना है, तो आपको CommandLineParser library में देखना होगा जो command line arguments को parse करने के लिए बहुत अधिक robust हो सकता है।
3. Implementation Details: C# में, 'Main' method की एक special version होती है जिसमें string array और 'args' parameter होते हैं। .Net runtime इन 'args' की values को set करता है जो कि command-line arguments होती है। 

## अधिक देखें:
१. Official Microsoft Documentation: [Main() and command-line arguments (C# Programming Guide)](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/main-and-command-args/)
२. Command Line Parser Library: [CommandLineParser](https://github.com/commandlineparser/commandline)