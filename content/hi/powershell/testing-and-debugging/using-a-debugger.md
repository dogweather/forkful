---
date: 2024-01-26 04:10:36.345711-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: PowerShell \u092E\
  \u0947\u0902, \u0906\u092A \u092C\u093F\u0932\u094D\u091F-\u0907\u0928 PowerShell\
  \ Integrated Scripting Environment (ISE) \u092F\u093E Visual Studio Code (VS Code)\
  \ \u0915\u0947 \u0938\u093E\u0925 PowerShell \u090F\u0915\u094D\u0938\u091F\u0947\
  \u0902\u0936\u0928 \u0915\u093E\u2026"
lastmod: '2024-03-13T22:44:52.710295-06:00'
model: gpt-4-0125-preview
summary: "PowerShell \u092E\u0947\u0902, \u0906\u092A \u092C\u093F\u0932\u094D\u091F\
  -\u0907\u0928 PowerShell Integrated Scripting Environment (ISE) \u092F\u093E Visual\
  \ Studio Code (VS Code) \u0915\u0947 \u0938\u093E\u0925 PowerShell \u090F\u0915\u094D\
  \u0938\u091F\u0947\u0902\u0936\u0928 \u0915\u093E \u0909\u092A\u092F\u094B\u0917\
  \ \u0915\u0930\u0915\u0947 \u0938\u094D\u0915\u094D\u0930\u093F\u092A\u094D\u091F\
  \u094D\u0938 \u0915\u094B \u0921\u0940\u092C\u0917 \u0915\u0930 \u0938\u0915\u0924\
  \u0947 \u0939\u0948\u0902\u0964 \u092F\u0939\u093E\u0902 \u0926\u094B\u0928\u094B\
  \u0902 \u092E\u0947\u0902 \u092C\u094D\u0930\u0947\u0915\u092A\u0949\u0907\u0902\
  \u091F \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0948\u0938\u0947 \u0915\
  \u0930\u0947\u0902 \u092C\u0924\u093E\u092F\u093E \u0917\u092F\u093E \u0939\u0948\
  ."
title: "\u0921\u0940\u092C\u0917\u0930 \u0915\u093E \u0909\u092A\u092F\u094B\u0917\
  \ \u0915\u0930\u0928\u093E"
weight: 35
---

## कैसे करें:
PowerShell में, आप बिल्ट-इन PowerShell Integrated Scripting Environment (ISE) या Visual Studio Code (VS Code) के साथ PowerShell एक्सटेंशन का उपयोग करके स्क्रिप्ट्स को डीबग कर सकते हैं। यहां दोनों में ब्रेकपॉइंट का उपयोग कैसे करें बताया गया है:

### PowerShell ISE:
```PowerShell
# एक निश्चित पंक्ति पर एक ब्रेकपॉइंट सेट करें
Set-PSBreakpoint -Script .\MyScript.ps1 -Line 5

# अपनी स्क्रिप्ट को सामान्य रूप से चलाएं
.\MyScript.ps1

# जब स्क्रिप्ट ब्रेकपॉइंट पर पहुँचती है, तो आप वेरिएबल की जांच कर सकते हैं
$myVariable

# निष्पादन जारी रखें
Continue
```

### Visual Studio Code:
```PowerShell
# अपनी PowerShell स्क्रिप्ट को VS Code में खोलें।
# लाइन नंबर के बाईं ओर क्लिक करके एक ब्रेकपॉइंट सेट करें।
# F5 दबाकर या 'Start Debugging' पर क्लिक करके डीबगिंग शुरू करें।

# VS Code आपके ब्रेकपॉइंट पर निष्पादन को रोक देगा।
# वेरिएबल्स को देखने, कॉल स्टैक की जांच करने, और फ्लो को नियंत्रित करने के लिए डीबग पैनल का उपयोग करें।
```

दोनों वातावरणों में डीबगिंग आपको डीबगिंग के दौरान अंदर (F11), ऊपर (F10), और बाहर (Shift+F11) चरणबद्ध तरीके से जाने देता है।

## गहराई में जानकारी
ऐतिहासिक रूप से, PowerShell में डीबगिंग थोड़ी क्लंकी थी; इसमें वेरिएबल की स्थितियां या पारंपरिक प्रयोग-और-त्रुटि विधि के माध्यम से आउटपुट के लिए बहुत सारी `Write-Host` लाइनों की आवश्यकता होती थी। PowerShell ISE के आगमन के साथ, और हाल ही में, अपनी समृद्ध डीबगिंग सुविधाओं के साथ VS Code के साथ, PowerShell डीबगिंग पूर्ण प्रोग्रामिंग भाषाओं के रूप में लगभग सहज हो गई।

PowerShell के मूल डीबगिंग टूल्स के लिए विकल्पों में तीसरे पक्ष के टूल जैसे कि PowerGUI शामिल हैं या PowerShell प्लगइन के साथ रोबस्ट IDEs जैसे कि Visual Studio का उपयोग करना शामिल है। 

एक डीबगर को लागू करते समय, विशेष रूप से डॉट-सोर्स्ड स्क्रिप्ट्स या मॉड्यूल के साथ काम करते समय, स्क्रिप्ट स्कोप पर विचार करें। ब्रेकपॉइंट्स स्थिति-आधारित, वेरिएबल परिवर्तन-आधारित, या पंक्ति-आधारित हो सकते हैं, जो एक डीबगिंग सत्र के दौरान सटीक नियंत्रण की अनुमति देते हैं।

इसके अलावा, PowerShell Core (क्रॉस-प्लेटफ़ॉर्म PowerShell) में संक्रमण के साथ, डीबगिंग मुख्य रूप से VS Code के हाथों में चली गई है, जो विभिन्न प्लेटफार्मों पर एक सुसंगत अनुभव प्रदान करती है।

## और देखें
PowerShell में डीबगिंग पर अधिक के लिए:
- [about_Debuggers](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_Debuggers)
