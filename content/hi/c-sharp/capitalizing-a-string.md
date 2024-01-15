---
title:                "स्ट्रिंग कैपिटलाइज़ बनाना"
html_title:           "C#: स्ट्रिंग कैपिटलाइज़ बनाना"
simple_title:         "स्ट्रिंग कैपिटलाइज़ बनाना"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्यों

कभी-कभी हमें एक स्ट्रिंग को कैपिटलाइज करने की जरूरत होती है ताकि यह पढ़ने में आसान हो और साथ ही यह दिखाई भी दे। इस लेख में हम सीखेंगे कि स्ट्रिंग को कैपिटलाइज कैसे किया जाता है और इसके दोनों तरीके क्या हैं।

## कैसे करें

```C#
string str = "hello world";

// Method 1: Using ToUpper() method
Console.WriteLine(str.ToUpper());
// Output: HELLO WORLD

// Method 2: Using CultureInfo
var cultureInfo = new CultureInfo("en-US", false);
var textInfo = cultureInfo.TextInfo;
Console.WriteLine(textInfo.ToTitleCase(str));
// Output: Hello World
```

## गहराई में जाएं

स्ट्रिंग कैपिटलाइज करने के दो साधारण तरीके हैं, जिनमें से पहला `ToUpper()` मेथड है जो स्ट्रिंग को ऑपरेशन करने के लिए मौजूद होता है। इसमें स्ट्रिंग को पूर्णत: अपरकेस में परिवर्तित किया जाता है। दूसरा तरीका `CultureInfo` का उपयोग करके किया जाता है, जिसमें हम `TextInfo` का उपयोग करके स्ट्रिंग को कैपिटलाइज कर सकते हैं। इससे स्ट्रिंग का पहला अक्षर हेडर केस में और बाकी लोअर केस में हो जाता है।

## देखें भी

- [String.ToUpper() method documentation](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupper)
- [TextInfo.ToTitleCase() method documentation](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.textinfo.totitlecase)