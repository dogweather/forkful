---
title:                "http अनुरोध भेजना"
html_title:           "Elixir: http अनुरोध भेजना"
simple_title:         "http अनुरोध भेजना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

HTTP अनुरोध भेजना ज्ञात होता है जब एक कंप्यूटर वेबसाइट के सर्वर से तथ्य या जानकारी मांगता है। प्रोग्रामर्स इसे करते हैं ताकि वे वेबसाइटों के साथ बातचीत का संपादन कर सकें, उनसे तथ्य प्राप्त करें और अद्यतित करें। 

## कैसे:

PowerShell का उपयोग करके HTTP अनुरोध भेजने के लिए, `Invoke-WebRequest` cmdlet का प्रयोग करें:

```PowerShell
$response = Invoke-WebRequest -Uri "http://example.com"
```

यह उदाहरण http://example.com से HTTP अनुरोध भेजता है और प्राप्त रिस्पांस को $response variable में संचित करता है।

## गहरी जानकारी:

HTTP अनुरोध की अवधारणा पहली बार 1991 में HTTP/1.0 विनिर्देश में परिचयित की गई थी। PowerShell में वैकल्पिक रूप से `Invoke-RestMethod` cmdlet भी उपयोग कर सकते हैं, जो JSON या XML रिस्पांस को स्वचालित रूप से पार्स करता है। `Invoke-WebRequest` और `Invoke-RestMethod` cmdlets, .NET Framework के System.Net.WebRequest class का उपयोग करते हैं HTTP संचार करने के लिए।

## अधिक जानने के लिए:

1. [Invoke-WebRequest आधिकारिक डॉक्युमेंटेशन](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7.1)
2. [Invoke-RestMethod आधिकारिक डॉक्युमेंटेशन](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod?view=powershell-7.1)
3. [.NET Framework HTTP विनिर्देश](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httprequestmessage?view=net-5.0)
4. [HTTP विनिर्देश](https://www.rfc-editor.org/rfc/rfc2616.html)