---
title:                "डिबग आउटपुट प्रिंट करना"
html_title:           "PowerShell: डिबग आउटपुट प्रिंट करना"
simple_title:         "डिबग आउटपुट प्रिंट करना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/printing-debug-output.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

डबग आउटपुट प्रिंट करना अर्थात अपने कोड के विवरण को दिखाना है। यह उन प्रोग्रामरों द्वारा किया जाता है जो अपने कोड में त्रुटियों को समझने और उन्हें ठीक करने में मदद करते हैं।

## कैसे करें:

डबग आउटपुट प्रिंट करने के लिए पावरशेल के "Write-Host" कमांड का उपयोग किया जाता है। इसमें आप अपनी पसंद के अनुसार टेक्स्ट या वेरिएबल्स को दर्शा सकते हैं। नीचे दिखाए गए कोड ब्लॉक आपको इसके उदाहरण देते हैं।

```PowerShell
Write-Host "Debug output" # अगर आप एक सामान्य टेक्स्ट या स्ट्रिंग प्रिंट करना चाहते हैं
Write-Host "Value of variable x is" $x # अगर आप किसी वेरिएबल का मान प्रिंट करना चाहते हैं
```

आप अपने कोड में अपनी पसंद के अनुसार कई जगह "Write-Host" कमांड का उपयोग कर सकते हैं और उससे आपको कोड के सभी चरणों पर खबर मिलती रहेगी।

## गहराई में जाएं:

डबग आउटपुट प्रिंट करने का इतिहास काफी धीमा है। पहले, प्रोग्रामरों को अपने कोड को डबग करने के लिए इस्तेमाल किया जाता था। परंतु, आजकल कई डेबगिंग टूल उपलब्ध हैं जो आपको अपने कोड को आसानी से डबग करने में मदद करते हैं। इसके अलावा, आप "Write-Host" कमांड के अलावा भी "Write-Verbose", "Write-Debug" और "Write-Error" जैसे अन्य कमांड्स भी उपयोग कर सकते हैं जो आपको अपने कोड के विभिन्न पहलुओं के उपर जानकारी देते हैं।

## अन्य संबद्ध स्रोत:

- [Microsoft द्वारा प्रस्तुत PowerShell डॉक्यूमेंटेशन](https://docs.microsoft.com/en-us/powershell/scripting/components/console/how-to-display-output-in-a-windows-powershell-console)
- [PowerShell "Write-Host" कमांड का उपयोग करना सीखें](https://www.tutorialspoint.com/powershell/powershell_write_host.htm)
- [PowerShell में ScriptDebug का उपयोग और इसके फायदे](https://devblogs.microsoft.com/scripting/powershell-scriptdebug-uses-benefits/)