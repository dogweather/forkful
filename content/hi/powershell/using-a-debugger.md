---
title:                "डीबगर का उपयोग करना"
date:                  2024-01-26T04:10:36.345711-07:00
model:                 gpt-4-0125-preview
simple_title:         "डीबगर का उपयोग करना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/using-a-debugger.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
डीबगर का उपयोग करने का मतलब है ब्रेकपॉइंट सेट करना, अपने कोड के माध्यम से चरणबद्ध तरीके से चलना, वेरिएबल को नज़र में रखना और आपके प्रोग्राम की स्थिति की जांच करना जैसे वह चल रहा हो। यह प्रोग्रामरों के लिए एक खेल बदलने वाला होता है क्योंकि यह बग्स को सटीक रूप से पहचानता है और हमें समझने में मदद करता है कि हमारा कोड वास्तव में क्या कर रहा है।

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
- [PowerShell डॉक्यूमेंटेशन ऑन डीबगिंग](https://docs.microsoft.com/en-us/powershell/scripting/debugging/debugging-in-powershell?view=powershell-7.2)
- [Visual Studio Code PowerShell एक्सटेंशन](https://marketplace.visualstudio.com/items?itemName=ms-vscode.PowerShell)