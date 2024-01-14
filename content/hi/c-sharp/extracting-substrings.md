---
title:    "C#: सबस्ट्रिंग्स को निकालना"
keywords: ["C#"]
---

{{< edit_this_page >}}

## क्यों

यदि आप C# प्रोग्रामिंग के बारे में कुछ जानते हैं, तो आपने शायद substring के बारे में सुना होगा। substring, कोड में एक छोटे से पार्ट को निकालने के लिए इस्तेमाल किया जाता है। इस लेख में, हम ऐसे substring को extract करना सीखेंगे जो कि किसी भी string में से निकाला जा सकता है।

## कैसे

```C#
string myString = "हिन्दी रीडर्स के लिए एक सी# प्रोग्रामिंग ब्लॉग पोस्ट।";
string substring = myString.Substring(0, 7);
Console.WriteLine(substring);
```

उपरोक्त कोड में, हमने ```Substring()``` फ़ंक्शन का उपयोग करके हमारे string के प्रथम 7 अक्षरों को extract किया है। इसके बाद, हमने उस substring को ```Console.WriteLine()``` का उपयोग करके दिखाया है। आपको उत्पन्न आउटपुट में निम्नलिखित को देखना चाहिए:

```
हिन्दी र
```

आप substring को किसी भी अन्य विधि से extract कर सकते हैं, जैसे कि index के द्वारा। अब चलिए, हम इसके बारे में थोड़ा और गहराई से जानते हैं। 

## गहराई से जानें

substring को extract करने के लिए, आपको C# में कई अलग-अलग मेथड हैं। जैसे कि हमने पहले देखा, आप ```Substring()``` फ़ंक्शन का उपयोग कर सकते हैं। इसके अलावा आप ```Substring()``` के overload विधियों, ```SubstringAle()```, ```SubstringIndex()``` और भी कई और फ़ंक्शन का उपयोग कर सकते हैं। आप इन सभी को [Microsoft की डॉक्यूमेंटेशन](https://docs.microsoft.com/en-us/dotnet/api/system.string.substring?view=netcore-3.1#System_String_Substring_System_Int32_System_Int32_) से जान सकते हैं।

## देखिये

[देखिए और सीखिए: C# में substring कैसे extract करें](https://www.geeksforgeeks.org/c-sharp-substring-method/) <br/>
[Microsoft की डॉक्यूमें