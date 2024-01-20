---
title:                "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
html_title:           "C#: बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

HTTP अनुरोध के साथ मूल प्रमाणीकरण भेजना सीधे API से डेटा प्राप्त करने की प्रक्रिया होती है जिसे हम PowerShel में कूड सकते हैं। प्रोग्रामर्स एक सुरक्षित तरीके से सर्वर तक पहुचने और डेटा मिलाने के लिए इसे इस्तेमाल करते हैं।

## कैसे:
```PowerShell
# प्रमाणीकरण हेडर की निर्माण
$User = 'username'
$Pass = 'password'

# पासवर्ड को सुरक्षित करे
$SecPass = ConvertTo-SecureString $Pass -AsPlainText -Force
$Cred = New-Object System.Management.Automation.PSCredential($User, $SecPass)

# HTTP अनुरोध बनाए
$R = Invoke-RestMethod -Uri 'https://your-api-url.com' -Method Get -Credential $Cred 
$R
```
आपका उत्तर प्राप्त हो सकता है जैसे-
```Shell
username
password
```

## गहरा विमर्श:
HTTP के साथ मूल प्रमाणीकरण का उपयोग 1991 में WWW द्वारा ऑनलाइन सन्देश प्रेषण करने के लिए किया गया था। अन्य विकल्पों में OAuth और Digest Access Authentication का उपयोग किया जाता है, पर मूलभूत प्रमाणीकरण की सरलता इसे लोकप्रिय रखती है। Invoke-RestMethod cmdlet इसे Powershell में बहुत आसानी से लागू करने की अनुमति देता है।

## अधिक जानकारी के लिए:
Invoke-RestMethod की विस्तार से जानकारी के लिए Microsoft दस्तावेज़: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod?view=powershell-7.1
HTTP Basic Authentication की विस्तार से जानकारी के लिए Mozilla Developer Network: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication