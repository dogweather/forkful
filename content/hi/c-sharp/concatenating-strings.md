---
title:    "C#: स्ट्रिंग्स को जोड़ना"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## क्यों
कोई भी व्यक्ति रीडिंग कон्केटनेटिंग स्ट्रिंग के लिए दृष्टिगत हो सकता है क्योंकि यह वेरिएबल्स को एक साथ जोड़ने के लिए एक सरल और भरोसेमंद तरीका है।

## कैसे करें
```C#
string str1 = "मैं";
string str2 = "एक";
string str3 = "कॉडर";
string res = str1 + str2 + str3;

Console.WriteLine(res);
```
उपरोक्त कोड के अनुसार, आपको स्ट्रिंग्स को जोड़ने के लिए अपने अवयवों को एक साथ जोड़कर एक उत्पाद वाली स्ट्रिंग का निर्माण करना होगा। इस उदाहरण को रन करने पर, आपको नया स्ट्रिंग `मैं एक कॉडर` मिलेगा।

## गहराई तक
स्ट्रिंग्स को जोड़ने के लिए, आपको ध्यान रखने की आवश्यकता है कि आपके द्वारा चयनित स्ट्रिंग्स का उपयोग आपके साथ किया जा सकता है, अन्यथा यह अमान्य स्ट्रिंग हो सकती है। इसके अलावा, आप विभिन्न कोडिंग स्टाइल का उपयोग करके स्ट्रिंग्स को जोड़ सकते हैं, जैसे शार्प ऑपरेटर (+) या `.Concat()` फ़ंक्शन।

## देखें भी
- [Microsoft डॉक्यूमेंटेशन आधिकारिक वेबसाइट](https://docs.microsoft.com/en-us/dotnet/api/system.string.concat?view=net-5.0)
- [तुलना अनुक्रम (Concatenation Operator)](https://www.tutorialspoint.com/csharp/csharp_string_concatenation.htm)
- [कॉनकेटनेशन और स्ट्रिंग वारिएबल्स (Concatenation and String Variables)](https://www.geeksforgeeks.org/c-sharp-concatenation-and-string-variables/)