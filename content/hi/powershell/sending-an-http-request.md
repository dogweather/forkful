---
title:                "HTTP अनुरोध भेजना"
html_title:           "PowerShell: HTTP अनुरोध भेजना"
simple_title:         "HTTP अनुरोध भेजना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
HTTP रिक्वेस्ट भेजना क्या होता है और क्यों कंप्यूटर प्रोग्रामर्स इसे करते हैं? एक HTTP रिक्वेस्ट एक ओर से सर्वर को डेटा भेजने और दूसरी ओर सर्वर से डेटा प्राप्त करने का एक तरीका है। कंप्यूटर प्रोग्रामर्स इस्तेमाल करें HTTP रिक्वेस्ट अपने प्रोग्राम में डेटा भेजने और प्राप्त करने के लिए क्योंकि यह एक सुरक्षित, तेजी से और आसान तरीका है।

## कैसे करें: 
```PowerShell
# एक वेब पेज से डेटा प्राप्त करना
Invoke-WebRequest -Uri "https://www.website.com" 

# पोस्ट डेटा भेजना 
Invoke-WebRequest -Method Post -Uri "https://www.website.com" -Body @{key1=value1;key2=value2}
```
इस उदाहरण में, हम `Invoke-WebRequest` फ़ंक्शन को उपयोग कर रहे हैं जो हमें किसी वेब पेज से डेटा प्राप्त करने और पोस्ट डेटा भेजने की अनुमति देता है। आप भी तुलना कर सकते हैं कि कैसे `Invoke-WebRequest` अपने अन्य कमांडो को, जैसे कि `Get-Content` और `Select-String`, से जुड़ता है। यह आपको विस्तार से जानकारी के लिए "डीप डाइव" सेक्शन में मिलेगा।

## डीप डाइव:
HTTP रिक्वेस्ट्स की एक लम्बी एतिहासिक पारम्परिक हैं। 1991 में, HTTP को प्रथम बार वैश्विक माध्यम बनाने के लिए अमेरिकी संगठन CERN में बनाया गया था। वैसे तो, कई अन्य भाषाओं में भी HTTP रिक्वेस्ट्स को भेजने के लिए है, जैसे कि Python में `requests` पैकेज और JavaScript में `fetch()` अपी। PowerShell इसमें बहुत आसानी से है क्‍योंकि यह Microsoft के द्वारा ड्वेलप किया गया है और Windows के साथ मौजूद है। आप इस लिंक के जरिए PowerShell और HTTP के बारे में अधिक सीख सकते हैं: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7.1.

## देखिये:
और सीखने के लिए, आप लिंक देख सकते हैं। एक और अच्‍छा स्रोत है: 
http://www.learnpowershell.com/http-requests-in-powershell/