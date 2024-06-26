---
date: 2024-01-20 18:04:50.790127-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902? (How to:) PowerShell\
  \ \u092E\u0947\u0902 \u0928\u092F\u093E \u092A\u094D\u0930\u094B\u091C\u0947\u0915\
  \u094D\u091F \u0936\u0941\u0930\u0942 \u0915\u0930\u0928\u093E \u092C\u0939\u0941\
  \u0924 \u0938\u0940\u0927\u093E \u0939\u0948\u0964 \u092E\u093E\u0928 \u0932\u0940\
  \u091C\u093F\u090F \u0906\u092A\u0915\u094B \u090F\u0915 \u0938\u094D\u0915\u094D\
  \u0930\u093F\u092A\u094D\u091F \u0915\u0947 \u0932\u093F\u090F \u0928\u092F\u093E\
  \ \u092B\u094B\u0932\u094D\u0921\u0930 \u092C\u0928\u093E\u0928\u093E \u0939\u0948\
  ."
lastmod: '2024-04-05T22:38:53.591436-06:00'
model: gpt-4-1106-preview
summary: ") PowerShell \u092E\u0947\u0902 \u0928\u092F\u093E \u092A\u094D\u0930\u094B\
  \u091C\u0947\u0915\u094D\u091F \u0936\u0941\u0930\u0942 \u0915\u0930\u0928\u093E\
  \ \u092C\u0939\u0941\u0924 \u0938\u0940\u0927\u093E \u0939\u0948\u0964 \u092E\u093E\
  \u0928 \u0932\u0940\u091C\u093F\u090F \u0906\u092A\u0915\u094B \u090F\u0915 \u0938\
  \u094D\u0915\u094D\u0930\u093F\u092A\u094D\u091F \u0915\u0947 \u0932\u093F\u090F\
  \ \u0928\u092F\u093E \u092B\u094B\u0932\u094D\u0921\u0930 \u092C\u0928\u093E\u0928\
  \u093E \u0939\u0948."
title: "\u0928\u0908 \u092A\u0930\u093F\u092F\u094B\u091C\u0928\u093E \u0936\u0941\
  \u0930\u0942 \u0915\u0930\u0928\u093E"
weight: 1
---

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
