---
title:                "स्ट्रिंग को छोटे अक्षरों में परिवर्तित करना"
aliases:
- /hi/c-sharp/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:38:23.332636-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग को छोटे अक्षरों में परिवर्तित करना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
हम strings को lower case में कैसे और क्यों बदलते हैं। पहला काम, यह दर्शाता है कि आप बिना किसी अंतर के, सभी टेक्स्ट को एक जैसा करना चाहते हैं। दूसरा, डेटा सुरक्षा और अद्वितीय पहचान प्रदान करना।

## How to: (कैसे करें:)
```C#
string originalString = "नमस्ते! कैसे हैं आप?";
string lowerCaseString = originalString.ToLower();

Console.WriteLine(originalString);   // Output: नमस्ते! कैसे हैं आप?
Console.WriteLine(lowerCaseString); // Output: नमस्ते! कैसे हैं आप?
```
नोट: उदाहरण में दी गई हिंदी स्ट्रिंग में अक्षर पहले से ही लोअर केस में हैं।

## Deep Dive (गहन अध्ययन)
`ToLower()` C# में एक string method है जो 2000s की देर से .NET Framework की शुरुआत से है। यह Culture-specific ढंग से काम करता है, तो हिंदी जैसी लोकलाइज़्ड लैंग्वेज में भी काम करेगा। 

`ToLowerInvariant()` विकल्प है जो कल्चर को अनदेखा करता है, यानी प्रोग्रामिंग वातावरण से बाहर एक निश्चित तरीके से काम करेगा। 

सही implementation का विचार क्या है? Unicode सीमा को ध्यान में रखना। सामान्य ASCII character सीधे-सीधे बदल जाते हैं, पर अन्य लैंग्वेज में special characters के नियम अलग होते हैं।

## See Also (और देखें)
- Microsoft's official documentation on ToLower: [Link](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower?view=net-6.0)
- Stack Overflow discussions on ToLower vs ToLowerInvariant: [Link](https://stackoverflow.com/questions/6225808/string-tolower-and-tolowerinvariant)
- Unicode standard for case mapping: [Link](https://www.unicode.org/reports/tr21/tr21-5.html)
