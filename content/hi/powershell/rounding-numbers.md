---
title:                "संख्याओं को पूर्णांक बनाना"
date:                  2024-01-26T03:47:04.107536-07:00
model:                 gpt-4-0125-preview
simple_title:         "संख्याओं को पूर्णांक बनाना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/rounding-numbers.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
संख्याओं को गोल करने का मतलब एक मूल्य को निकटतम पूर्णांक या निर्दिष्ट दशमलव स्थान पर समायोजित करना है। प्रोग्रामर संख्याओं को डेटा को सरल बनाने, पठनीयता बढ़ाने या गणना के दौरान कुछ गणितीय आवश्यकताओं को पूरा करने के लिए गोल करते हैं।

## कैसे:
आपके पास PowerShell में संख्याओं को गोल करने के लिए कुछ सहायक cmdlets और विधियाँ हैं:

- मैथ कक्षा से `Round()` विधि
```PowerShell
[Math]::Round(15.68) # 16 को गोल करता है
```
- दशमलव निर्दिष्ट करें:
```PowerShell
[Math]::Round(15.684, 2) # 15.68 को गोल करता है
```
- `Ceiling()` और `Floor()`, हमेशा ऊपर या नीचे की ओर गोल करने के लिए:
```PowerShell
[Math]::Ceiling(15.2) # 16 की ओर ऊपर को गोल करता है
[Math]::Floor(15.9) # 15 की ओर नीचे को गोल करता है
```

## गहराई से जानकारी
संख्याओं को गोल करना कोई नई बात नहीं है; यह प्राचीन समय से ही रहा है, व्यापार, विज्ञान, और समय निर्धारण के लिए उपयोगी रहा है। PowerShell के बारे में बात करें तो, `[Math]::Round()` डिफ़ॉल्ट रूप से "Banker's Rounding" का पालन करता है, जहाँ 0.5s निकटतम सम पूर्णांक की ओर जाता है, सांख्यिकीय संचालनों में पूर्वाग्रह को कम करता है।

आप केवल `[Math]` विधियों के साथ ही सीमित नहीं हैं। अधिक नियंत्रण चाहिए? `[System.Math]::Round(Number, Digits, MidpointRounding)` की जाँच करें जहाँ आप निर्धारित कर सकते हैं कि मध्यबिंदु संभालने कैसे हैं: शून्य से दूर या सम (अर्थात Banker’s Rounding) के लिए।

एक और दृष्टिकोण: `System.Globalization.CultureInfo` ऑब्जेक्ट। जब अंतरराष्ट्रीय संख्याओं के साथ काम करते समय स्थान-विशिष्ट प्रारूपण और गोल करने की प्राथमिकताओं में मदद करता है।

## भी देखें
- Math विधियों पर Microsoft के आधिकारिक दस्तावेज: [लिंक](https://learn.microsoft.com/en-us/dotnet/api/system.math?view=net-7.0)
- .NET में दशमलव गोल करने की विशिष्टताएँ: [लिंक](https://learn.microsoft.com/en-us/dotnet/api/system.midpointrounding?view=net-7.0)
- StackOverflow में गोल करने पर चर्चा: [लिंक](https://stackoverflow.com/questions/tagged/rounding+powershell)