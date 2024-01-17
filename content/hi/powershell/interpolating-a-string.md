---
title:                "स्ट्रिंग को इंटरपोलेट करना"
html_title:           "PowerShell: स्ट्रिंग को इंटरपोलेट करना"
simple_title:         "स्ट्रिंग को इंटरपोलेट करना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
संवर्धन एक स्ट्रिंग में भेजे हुए वेरिएबल्स और चिन्हों का उपयोग करके उनमें दिए गए मानों को डालने की प्रक्रिया है। इस प्रक्रिया का उपयोग प्रोग्रामर्स वैबसाइट्स, दस्तावेज और अन्य प्रोग्रामिंग परियोजनाओं में डाटा स्ट्रक्चर को त्वरित ढंग से संपादित करने के लिए करते हैं।

## कैसे करें:
```PowerShell
$name = "जॉन"
Write-Host "नमस्ते, $name! आप कैसे हो?"
```
**आउटपुट:** नमस्ते, जॉन! आप कैसे हो?

## गहराई में जाएँ:
1. ऐतिहासिक पृष्ठभूमि: संवर्धन 1970 के दशक में फॉरच नामक प्रोग्रामिंग भाषा में पेश किया गया था। लेकिन, यह आजकल अन्य प्रोग्रामिंग भाषाओं में भी प्रचलित है।
2. वैकल्पिक विकल्प: संवर्धन का एक और विकल्प है हार्ड कोडिंग, जिसमें स्ट्रिंग में हार्ड कोडिंग के रूप में मान दिए जाते हैं। लेकिन, यह अप्रभावी और दोहराने लायक हो सकता है।
3. अंतरण विवरण: संवर्धन को विभिन्न भाषाओं, उदाहरण के लिए C#, Java, और Python में अलग-अलग तरीकों से अंतर्गत किया जा सकता है। यह तकनीक स्ट्रिंग को बनाने, प्रकारभेद करने और ताजगी देने में मदद करती है।

## आगे देखें:
- [The Basics of String Interpolation in C#](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/tokens/interpolated)
- [String Formatting in Java](https://docs.oracle.com/javase/tutorial/java/data/numberformat.html)
- [Python String Formatting](https://docs.python.org/3/reference/lexical_analysis.html#f-strings)