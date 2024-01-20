---
title:                "डीबग आउटपुट प्रिंट करना"
html_title:           "Gleam: डीबग आउटपुट प्रिंट करना"
simple_title:         "डीबग आउटपुट प्रिंट करना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/printing-debug-output.md"
---

{{< edit_this_page >}}

## क्या और क्यों? 
Debug आउटपुट प्रिंट करना, यह है की आपके कोड का कुछ विशेष भाग को चेक करने के लिए टर्मिनल पर संदेश प्रिंट करना। इसका मुख्य उद्देश्य यह होता है कि प्रोग्रामर को उसके कोड की कार्यक्षमता और व्यवहार को समझने में मदद मिल सके।

## कैसे करें: 
चलिए देखते हैं कि PowerShell में debug आउटपुट को कैसे प्रिंट करते हैं। 
```PowerShell
# Debug Message को प्रिंट करने के लिए Write-Debug cmdlet का उपयोग करें
Write-Debug "Debug मेसेज: यहाँ कुछ कोड होगा"

# इसे रन करने पर आपको कुछ नहीं दिखाई देगा, जब तक की $DebugPreference इसे "Continue" नहीं सेट करता है
$DebugPreference = "Continue"
Write-Debug "Debug मेसेज: यहाँ कुछ कोड होगा"
```
जब आप इसे रन करेंगे, आपको दूसरी लाईन 'Debug message: here would be some code' को दिखाया जाएगा।

## गहरा ज्ञान 
Debug आउटपुट हमेशा से ही कोड डीबग करने और त्रुटियों को पहचानने के लिए एक महत्वपूर्ण उपकरण रहा है। PowerShell में `Write-Debug` cmdlet पहले से ही मौजूद है और यह डेवलपर्स को डीबग जानकारी को प्रिंट करने में सहायता करता है। 
वैकल्पिक तौर पर, `Write-Verbose` और `Write-Warning` भी उपयोगी हो सकते हैं, आवश्यकता के आधार पर। 
प्रत्येक cmdlet में अपने खुद के पहचानकर्ता या दृष्टिकोण होते हैं, और यही सबसे अच्छी बात है इन cmdlets की।
यह इनका इस्तेमाल बहुत ही विविध और आसान बना देता है।

## और भी देखें: 
2. [PowerShell Write-Debug (Microsoft Docs)](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/write-debug?view=powershell-7)