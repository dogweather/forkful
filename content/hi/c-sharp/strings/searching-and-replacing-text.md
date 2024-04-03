---
date: 2024-01-20 17:57:57.065495-07:00
description: "How To: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) ."
lastmod: '2024-03-13T22:44:52.305866-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u092A\u093E\u0920 \u0916\u094B\u091C\u0928\u093E \u0914\u0930 \u092C\u0926\
  \u0932\u0928\u093E"
weight: 10
---

## How To: (कैसे करें:)
```C#
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string originalText = "हम C# सीख रहे हैं। C# मजेदार है।";
        string searchText = "C#";
        string replaceText = "C#.NET";

        // सिंपल रिप्लेसमेंट
        string replacedText = originalText.Replace(searchText, replaceText);
        Console.WriteLine(replacedText);

        // Regex के जरिये रिप्लेसमेंट
        string regexReplacedText = Regex.Replace(originalText, searchText, replaceText);
        Console.WriteLine(regexReplacedText);
    }
}
```
सैंपल आउटपुट:
```
हम C#.NET सीख रहे हैं। C#.NET मजेदार है।
हम C#.NET सीख रहे हैं। C#.NET मजेदार है।
```

## Deep Dive (गहराई में जानकारी):
टेक्स्ट की सर्च और रिप्लेसमेंट की जरूरत पुराने प्रोग्राम्स में भी थी, जैसे कि पुराने टेक्स्ट एडिटर्स। हालाँकि, आज यह ऑपरेशन ज्यादा सोफिस्टिकेटेड हो गया है और Regex (Regular Expressions) के इस्तेमाल से बहुत फ्लेक्सिबल भी। रेगुलर एक्सप्रेशन का इस्तेमाल करके आप कॉम्प्लेक्स पैटर्न्स को भी सर्च और रिप्लेस कर सकते हैं। इसके अल्टरनेटिव में कई लाइब्रेरीज और टूल्स हैं जैसे कि sed (stream editor) जो लिनक्स पर यूज होता है। C# में `String.Replace` मेथड सीधा और सरल है, जबकि `Regex.Replace` जटिल पैटर्न्स के साथ काम कर सकता है।

## See Also (और जानें):
- Microsoft Docs on String.Replace method: [Microsoft Docs](https://docs.microsoft.com/dotnet/api/system.string.replace)
- Regular Expressions in .NET: [Regular Expressions](https://docs.microsoft.com/dotnet/standard/base-types/regular-expressions)
- .NET Regex.Replace method: [Regex.Replace](https://docs.microsoft.com/dotnet/api/system.text.regularexpressions.regex.replace)
