---
title:    "C#: नियमित अभिव्यक्तियों का उपयोग"
keywords: ["C#"]
---

{{< edit_this_page >}}

## क्यों

क्या आपने कभी सोचा है कि आपके पास उच्चतम स्तर के स्ट्रिंग मेनिपुलेशन के लिए एक उपकरण हो सकता है? यदि हाँ, तो आपको रेगुलर एक्सप्रेशन इस्तेमाल करना चाहिए। इससे आपको सुरक्षा, वैधता और व्यवस्थितता के लिए अधिक सुरक्षित कोड प्राप्त होता है। इसलिए, रेगुलर एक्सप्रेशन का इस्तेमाल आपको अधिक प्रोफेशनल दिखने में मदद कर सकता है।

## कैसे करें

चलिए, अब हम देखें कि रेगुलर एक्सप्रेशन को कैसे उपयोग किया जाता है। यहाँ हम दो तरह के साधन देखेंगे: ```Match``` और ```Regex```.

### ```Match```

```C#
string input = "Hello C# programmers!";
string pattern = @"C#";

Match match = Regex.Match(input, pattern);

if (match.Success)
{
    Console.WriteLine("Found a match for C#!");
}
```

यहाँ हमने ```Match``` के साथ एक सेल्फ-एग्ज्प्लेनेटरी कोड दिखाया है। हमने यहाँ एक स्ट्रिंग में रहते हुए पैटर्न की खोज की है और यदि सफल होता है तो उसे को Boolean रत्न में ले आया है।

### ```Regex```

```C#
string input = "Hello C# programmers!";
string pattern = @"C#";

Regex regex = new Regex(pattern);
Match match = regex.Match(input);

if (match.Success)
{
    Console.WriteLine("Found a match for C#!");
}
```

यह उदाहरण भी ```Match``` के साथ बहुत समान है। एकमात्र अंतर यह है कि ```Regex``` आपको रेगुलर एक्सप्रेशन ऑब्जेक्ट को पहले बनाने की अनुमति देता है। इससे आप एक बार बनाए गए ऑब्जेक्ट का पुनरावर्ती इस्तेमाल कर सकते हैं जब आपको रक्त दोहन करना पड़ता है।

## डीप डाइव

रेगुलर एक्सप्रेशन का अधिक गहराई से समझना अपेक्षित है क्योंकि यह