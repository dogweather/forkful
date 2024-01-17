---
title:                "टेक्स्ट को खोजना और बदलना"
html_title:           "PowerShell: टेक्स्ट को खोजना और बदलना"
simple_title:         "टेक्स्ट को खोजना और बदलना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
सर्च और रिप्लेस टेक्स्ट क्या है, और प्रोग्रामर्स इसे क्यों करते हैं? सर्च और रिप्लेस टेक्स्ट भाषा में भिन्नताएं ढूंढने और उन्हें दूसरी भाषा में बदलने का एक तरीका है। प्रोग्रामर्स इसका उपयोग कोड के सुधार और लेखों में त्रुटियों को ढूंढने और सुधार के लिए करते हैं।

## कैसे करें?
```PowerShell
# टेक्स्ट सर्च और रिप्लेस करने के लिए आवश्यक कोड
Get-Content file.txt | ForEach-Object { $_ -replace "भाषा", "language" } | Set-Content file.txt
```
इस उदाहरण में, हमने ```file.txt``` नामक एक फाइल के सामग्री को पढ़कर, हर पंक्ति में "भाषा" को "language" में बदल दिया है। परिवर्तन उसी फाइल में रखा गया है।

## गहराई में जाएं
सर्च और रिप्लेस टेक्स्ट का इतिहास बहुत पुराना है। शुरू में, प्रोग्रामर्स इसे स्वयं करते थे अथवा विभिन्न शैलियों में कॉडिंग के साथ समय अवधारणा करते थे। हालांकि, आजकल कई एडिटर और आधुनिक IDE में इस टेक्स्ट फीचर उपलब्ध है। अन्य समाधानों में, विभिन्न कोडिंग भाषाओं के लिए विशेष टेक्स्ट एडिटर भी सर्च और रिप्लेस के लिए उपलब्ध हैं। टेक्स्ट सर्च और रिप्लेस का मुख्य फायदा है कि यह दृष्टिकोण और ढांचे को बदले बिना, कोड को सुधारने में मदद करता है। 

## अन्य संबंधित स्रोत
टेक्स्ट सर्च और रिप्लेस के लिए PowerShell में कुछ और विशेषताएं हैं, जैसे Regular Expression (सामान्य अभिव्यक्ति) और -match या -like पैरामीटर। अधिक जानकारी के लिए, निम्नलिखित स्रोतों को देखें:
- [PowerShell's official documentation](https://docs.microsoft.com/en-us/powershell/scripting/getting-started/cookbooks/working-with-text-based-data-files?view=powershell-7)
- [PowerTip: Use PowerShell to search and replace text](https://devblogs.microsoft.com/scripting/powertip-use-powershell-to-search-and-replace-text)
- [PowerShell Techniques: File Substitution with RegEx and Set-Content](https://www.red-gate.com/simple-talk/sysadmin/powershell/powershell-techniques-file-substitution-with-regex-and-set-content)