---
title:                "स्ट्रिंग्स को जोड़ना"
html_title:           "Bash: स्ट्रिंग्स को जोड़ना"
simple_title:         "स्ट्रिंग्स को जोड़ना"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

# प्रोग्रामिंग में स्ट्रिंग्स को कोन्केटिनेट करना

## क्या और क्यों?
स्ट्रिंग्स को कोन्केटिनेट करना मतलब होता है की आप दो या दो से अधिक स्ट्रिंग्स को एक साथ मिला रहे हो. प्रोग्रामर्स इस काम कोतब करते हैं जबब उन्हें विशेष संदेश बनाने, चर मान्यताएं देखने या डाटा को एक साथ दिखाने की आवश्यकता होती है।

## कैसे करें:
C# में, आप `+` ऑपरेटर का उपयोग करके स्ट्रिंग्स को कोन्केटिनेट कर सकते हैं:

```C#
string firstName = "Shyam";
string lastName = "Sharma";
string fullName = firstName + " " + lastName;
Console.WriteLine(fullName); // आउटपुट: "Shyam Sharma"
```

## गहरी डाइव
स्ट्रिंग कोन्केटिनेशन का इतिहास साधारण यंत्रालो के समय से ही शुरू हुआ। C# में, एल्टरनेटिव के रूप में, आप `StringBuilder` का उपयोग कर सकते हैं जो पहले के तुलना में अधिक कुशल और तेज़ हो सकता है अगर आप कई स्ट्रिंग को कोन्केटिनेट कर रहे हो:

```C#
StringBuilder sb = new StringBuilder();
sb.Append("Shyam");
sb.Append(" ");
sb.Append("Sharma");
Console.WriteLine(sb.ToString()); // आउटपुट: "Shyam Sharma"
```

## देखें भी:
- [Microsoft Documentation on String concatenation](https://docs.microsoft.com/en-us/dotnet/csharp/how-to/concatenate-multiple-strings)
- [StringBuilder Class](https://docs.microsoft.com/en-us/dotnet/api/system.text.stringbuilder?view=net-5.0)
- [C# String Manipulation](https://www.c-sharpcorner.com/article/C-Sharp-string-manipulation/)