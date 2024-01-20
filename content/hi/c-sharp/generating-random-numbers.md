---
title:                "यादृच्छिक संख्याओं का निर्माण"
html_title:           "Clojure: यादृच्छिक संख्याओं का निर्माण"
simple_title:         "यादृच्छिक संख्याओं का निर्माण"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## व्हाट एंड वाय? 
रैंडम नंबर जनरेशन एक प्रक्रिया होती है जिसमें हम कार्यक्रम के प्रत्येक चलन पर अद्वितीय और अनपेक्षित संख्याएं उत्पन्न करते हैं। यह गेमिंग, एन्क्रिप्शन, सिमुलेशन, आदि में उपयोग होता है जहां आकस्मिकता की आवश्यकता होती है।

## हाउ टू: 
C# में, `Random` क्लास का उपयोग करके रैंडम नंबर उत्पन्न किए जा सकते हैं।

```C#
using System;

class Program
{
  static void Main()
  {
    Random randNum = new Random();
    Console.WriteLine(randNum.Next());
  }
}
```
आउटपुट हर बार अलग होगा, क्योंकि हर बार एक नयी रैंडम नंबर जनित होता है।

## डीप डाइव: 
### ऐतिहासिक संदर्भ: 
रैंडम नंबर जनरेशन की कड़ी अनुसंधान की साथ शुरू हुई, और इसने सूचना विज्ञान, गणित, खेल और अन्य क्षेत्रों में महत्वपूर्ण भूमिका निभाई। 

### विकल्प: 
`Random` क्लास के विकल्प में `RNGCryptoServiceProvider` आता है, जो क्रिप्टोग्राफिक IPPROTO के लिए मजबूत रैंडम नंबर प्रदान करता है। 

### विषयानुवादन विवरण: 
`Random` क्लास C# में एक प्स्यूडो-रैंडम नंबर जनरेटर होता है, जिसका आधार एक आंतरिक काउंटर के उपर होता है। 

## देखें भी: 
- [Random Number Generation Tutorial in C#](https://docs.microsoft.com/en-us/dotnet/api/system.random?view=net-5.0)
- [RNGCryptoServiceProvider Class in .Net](https://docs.microsoft.com/en-us/dotnet/api/system.security.cryptography.rngcryptoserviceprovider?view=net-5.0)