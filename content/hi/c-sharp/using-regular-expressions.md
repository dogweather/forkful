---
title:                "रेगुलर एक्सप्रेशन का उपयोग"
date:                  2024-01-19
html_title:           "Bash: रेगुलर एक्सप्रेशन का उपयोग"
simple_title:         "रेगुलर एक्सप्रेशन का उपयोग"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
Regular expressions (regex) का इस्तेमाल टेक्स्ट को पैटर्न के अनुसार खोजने, बदलने या उस पर ऑपरेशन करने के लिए किया जाता है। Programmers इसका इस्तेमाल इसलिए करते हैं क्योंकि यह जटिल टेक्स्ट प्रोसेसिंग टास्क को आसान और जल्दी हल करने में मदद करता है। 

## How to: (कैसे करें:)
साधारण regex पैटर्न के इस्तेमाल का उदाहरण:
```C#
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string text = "मेरा ईमेल है example@domain.com";
        string pattern = @"\w+@\w+\.\w+"; // ईमेल पते के लिए regex pattern

        Match match = Regex.Match(text, pattern);

        if (match.Success)
        {
            Console.WriteLine("मिला हुआ ईमेल: " + match.Value);
        }
    }
}
```
Sample Output:
```
मिला हुआ ईमेल: example@domain.com
```

## Deep Dive (गहराई में जानकारी):
Regex का प्रयोग प्रोग्रामिंग में 1950 के दशक से होता आया है। इसके विकल्प के रूप में string मेथोड्स का उपयोग हो सकता है लेकिन वे regex की तरह शक्तिशाली और सुगम नहीं हैं। C# में, regex की कार्यान्वयन `System.Text.RegularExpressions` नामस्थान (namespace) में होती है और यह .NET Framework का भाग है, जोकि powerful regex engine प्रदान करता है।

## See Also (इसे भी देखें):
- [.NET डॉक्स पर Regular Expressions](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions)
- [Regex101: ऑनलाइन regex tester और debugger](https://regex101.com/)
- [Regex सीखने के लिए interactive exercises](https://regexone.com/)
