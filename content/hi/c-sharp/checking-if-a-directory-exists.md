---
title:    "C#: डायरेक्टरी मौजूद है या नहीं की जाँच करना"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## क्यों

यदि आप अपने प्रोग्राम में फ़ाइल या फ़ोल्डर को खोजना चाहते हैं, तो आपको पहले से ही जाँचना चाहिए कि क्या यह फ़ाइल या फ़ोल्डर मौजूद है। यह सुनिश्चित करना आपको समय और उपयोगकर्ता को परेशानी से बचाने में मदद कर सकता है।

## कैसे करें

आपको `Directory.Exists()` मेथड का उपयोग करके फ़ाइल या फ़ोल्डर की मौजूदगी को जाँच सकते हैं। नीचे दिए गए कोड ब्लॉक में एक उदाहरण है। 

```C#
string path = @"C:\Users\Username\Desktop\NewFolder";

if (Directory.Exists(path))
{
    Console.WriteLine("Directory already exists.");
}
else
{
    Console.WriteLine("Directory does not exist.");
}
```

यहाँ, हमने `Directory.Exists()` मेथड की मदद से `path` में दिए गए फ़ोल्डर की मौजूदगी को जाँचा है। अगर फ़ोल्डर मौजूद होता है तो `Directory.Exists()` मेथड `true` लौटा देता है और हम इसे `if` शर्त में उपयोग करके संदेश दिखा सकते हैं। अगर फ़ोल्डर मौजूद नहीं होता है तो `Directory.Exists()` मेथड `false` लौटा देता है और हम इसे `else` शर्त में उपयोग करके दूसरा संदेश दिखा सकते हैं। 

## गहराई में जाएं

अब आप जानते हैं कि कैसे फ़ाइल या फ़ोल्डर की मौजूदगी को जाँचना है, लेकिन `Directory.Exists()` मेथड कुछ अन्य मेथड्स और प्रॉपर्टीज के साथ मिलकर काम करता है। अधिक जानकारी के लिए, आप माइक्रोसॉफ्ट दस्तावेज़ देख सकते हैं।

## देखें भी

- [Directory Class (C# Programming Guide)](https://docs.microsoft.com/en-us/dotnet/api/system.io.directory?view=netframework-4.7.2)
- [Exists(String)](https