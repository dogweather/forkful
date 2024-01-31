---
title:                "नई परियोजना शुरू करना"
date:                  2024-01-20T18:04:50.790127-07:00
model:                 gpt-4-1106-preview
simple_title:         "नई परियोजना शुरू करना"

category:             "PowerShell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
नया प्रोजेक्ट शुरू करना एक खाली कैनवास पर चित्र बनाने जैसा है। प्रोग्रामर्स इसलिए नए प्रोजेक्ट्स शुरू करते हैं क्योंकि उन्हें नई समस्याओं का समाधान करना होता है या नए आइडियाज पर काम करना होता है।

## कैसे करें? (How to:)
PowerShell में नया प्रोजेक्ट शुरू करना बहुत सीधा है। मान लीजिए आपको एक स्क्रिप्ट के लिए नया फोल्डर बनाना है:

```PowerShell
# नया फोल्डर बनाने के लिए New-Item इस्तेमाल करें:
New-Item -Path 'C:\MyNewProject' -ItemType Directory

# फोल्डर की सूची देखें:
Get-ChildItem -Path 'C:\'
```

आउटपुट होगा कुछ इस प्रकार:

```
    Directory: C:\
Mode                LastWriteTime         Length Name
----                -------------         ------ ----
d-----        1/1/2023   9:00 AM                MyNewProject
```

## गहराई से जानकारी (Deep Dive)
PowerShell अपने शुरूआती दिनों से ही स्क्रिप्टिंग और ऑटोमेशन के लिए लोकप्रिय है। पहले बैच फाइल्स और VBScript का इस्तेमाल होता था, लेकिन PowerShell ने इसे आसान और शक्तिशाली बनाया। नए प्रोजेक्ट्स के लिए आप Module, Script, Manifest जैसे संसाधनों का ढांचा बना सकते हैं। प्रत्येक की अपनी अहमियत है:

- **Module**: पुन: प्रयोग योग्य फंक्शन्स और वेरिएबल्स का सेट।
- **Script**: निष्पादन योग्य पावरशेल स्क्रिप्ट।
- **Manifest**: मॉड्यूल मेटाडेटा।

किसी भी नए प्रोजेक्ट को शुरू करने से पहले, यह ध्यान में रखना अच्छा है कि आप कोड को कैसे संरचना देना चाहते हैं और क्या आप भविष्य में इसके पुन: उपयोग का सोच रहे हैं।

## संबंधित सोर्सेज (See Also)
- [PowerShell Documentation](https://docs.microsoft.com/en-us/powershell/)
- [PowerShell Scripting Guide](https://docs.microsoft.com/en-us/powershell/scripting/overview)
- [About Modules (PowerShell)](https://docs.microsoft.com/en-us/powershell/scripting/developer/module/understanding-a-windows-powershell-module)
