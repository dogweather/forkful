---
title:                "स्ट्रिंग को कैपिटलाइज करना"
html_title:           "C#: स्ट्रिंग को कैपिटलाइज करना"
simple_title:         "स्ट्रिंग को कैपिटलाइज करना"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
"स्ट्रिंग को capitalise करना" का मतलब होता है कि स्ट्रिंग के पहले अक्षर को बड़ा बनाना। प्रोग्रामर इसे सरलता और स्पष्टता के लिए करते हैं, जैसे कि नाम या शीर्षक में.

## कैसे करें:
यहां एक उदाहरण दिया गया है कि कैसे हम C# में स्ट्रिंग को capitalise कर सकते हैं:

```C#
string inputStr = "hello world";
TextInfo myTI = new CultureInfo("en-US", false).TextInfo;
string outputStr = myTI.ToTitleCase(inputStr);
Console.WriteLine(outputStr);
```
इसका आउटपुट होगा:
```
Hello World
```

## गहराई से:
स्ट्रिंग को Capitalise करने का विचार पहले से ही तब आया जब डिजिटल टेक्स्ट का उपयोग किया गया था। वैकल्पिक तरिके जैसे कि Substring() या Char.ToUpper() का उपऔग भी किया जा सकता है, लेकिन ToTitleCase() एक सामर्थ्यवान और क्षेत्रीय सेटिंग्स को सम्मानित करता है।

## और भी देखें:
Capitalisation के बारे में अधिक जानने के लिए, आप इन लिंक्स पे जाएँ:
- Microsoft's TitleCase: https://docs.microsoft.com/en-us/dotnet/api/system.globalization.textinfo.totitlecase?view=net-5.0
- Stackoverflow Discussion on Capitalisation in C#: https://stackoverflow.com/questions/4135317/make-first-letter-of-a-string-upper-case-with-maximum-performance