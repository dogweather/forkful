---
title:                "C#: स्ट्रिंग को कैपिटलाइज़ करना"
simple_title:         "स्ट्रिंग को कैपिटलाइज़ करना"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्यों

कोई भी किसी स्ट्रिंग को कैपिटलाइज करने में क्यों रुचि रखेगा, इसका कारण सुनिश्चित रूप से लोगों को उस लेख में दिये गए कारणों से समझ लेना चाहिए।

## कैसे

छोटा उदाहरण के साथ स्ट्रिंग कैपिटलाइज करने के तरीके: "```C#
string str = "hello world";
Console.WriteLine(str.ToUpper());
```
आउटपुट: "HELLO WORLD" 

लम्बा तरीके से स्ट्रिंग कैपिटलाइज करने का उपयोग: "```C#
string str = "hello world";
string capStr = CultureInfo.CurrentCulture.TextInfo.ToTitleCase(str);
Console.WriteLine(capStr);
```
आउटपुट: "Hello World"

## गहराई में जाइए

स्ट्रिंग कैपिटलाइज का सामान्य उपयोग है डिसप्ले करने में ध्यान आकर्षित करने के लिए। इससे आप अपने UI में शिर्षक, स्लोगन, या आस्ते हिसाब से स्ट्रिंग्ज दिखा सकते हैं। आप भाषाई अनुवाद के साथ स्ट्रिंग क्षमताओं में भी कैपिटलाइज कर सकते हैं जो कि प्रयोगकर्ता को अधिक समझ महसूस कराते हैं।

## देखिए भी

- [C# String Class](https://docs.microsoft.com/en-us/dotnet/api/system.string?view=netcore-3.1)
- [ToTitleCase() Method](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.textinfo.totitlecase?view=netcore-3.1)
- [String Manipulation in C#](https://www.geeksforgeeks.org/c-sharp-string-manipulation/)