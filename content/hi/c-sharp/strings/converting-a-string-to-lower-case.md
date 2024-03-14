---
date: 2024-01-20 17:38:23.332636-07:00
description: "\u0939\u092E strings \u0915\u094B lower case \u092E\u0947\u0902 \u0915\
  \u0948\u0938\u0947 \u0914\u0930 \u0915\u094D\u092F\u094B\u0902 \u092C\u0926\u0932\
  \u0924\u0947 \u0939\u0948\u0902\u0964 \u092A\u0939\u0932\u093E \u0915\u093E\u092E\
  , \u092F\u0939 \u0926\u0930\u094D\u0936\u093E\u0924\u093E \u0939\u0948 \u0915\u093F\
  \ \u0906\u092A \u092C\u093F\u0928\u093E \u0915\u093F\u0938\u0940 \u0905\u0902\u0924\
  \u0930 \u0915\u0947, \u0938\u092D\u0940 \u091F\u0947\u0915\u094D\u0938\u094D\u091F\
  \ \u0915\u094B \u090F\u0915 \u091C\u0948\u0938\u093E \u0915\u0930\u0928\u093E \u091A\
  \u093E\u0939\u0924\u0947 \u0939\u0948\u0902\u0964 \u0926\u0942\u0938\u0930\u093E\
  , \u0921\u0947\u091F\u093E\u2026"
lastmod: '2024-03-13T22:44:52.309347-06:00'
model: gpt-4-1106-preview
summary: "\u0939\u092E strings \u0915\u094B lower case \u092E\u0947\u0902 \u0915\u0948\
  \u0938\u0947 \u0914\u0930 \u0915\u094D\u092F\u094B\u0902 \u092C\u0926\u0932\u0924\
  \u0947 \u0939\u0948\u0902\u0964 \u092A\u0939\u0932\u093E \u0915\u093E\u092E, \u092F\
  \u0939 \u0926\u0930\u094D\u0936\u093E\u0924\u093E \u0939\u0948 \u0915\u093F \u0906\
  \u092A \u092C\u093F\u0928\u093E \u0915\u093F\u0938\u0940 \u0905\u0902\u0924\u0930\
  \ \u0915\u0947, \u0938\u092D\u0940 \u091F\u0947\u0915\u094D\u0938\u094D\u091F \u0915\
  \u094B \u090F\u0915 \u091C\u0948\u0938\u093E \u0915\u0930\u0928\u093E \u091A\u093E\
  \u0939\u0924\u0947 \u0939\u0948\u0902\u0964 \u0926\u0942\u0938\u0930\u093E, \u0921\
  \u0947\u091F\u093E\u2026"
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u091B\u094B\
  \u091F\u0947 \u0905\u0915\u094D\u0937\u0930\u094B\u0902 \u092E\u0947\u0902 \u092A\
  \u0930\u093F\u0935\u0930\u094D\u0924\u093F\u0924 \u0915\u0930\u0928\u093E"
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
