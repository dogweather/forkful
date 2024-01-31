---
title:                "वर्तमान तारीख प्राप्त करना"
date:                  2024-01-20T15:16:40.339463-07:00
simple_title:         "वर्तमान तारीख प्राप्त करना"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

PowerShell में वर्तमान तारीख प्राप्त करना एक साधारण प्रक्रिया है; यह आम तौर पर लॉगिंग, समय-संबंधित कार्यों, या सिस्टम मॉनिटरिंग स्क्रिप्ट्स में प्रयोग होता है। प्रोग्रामर्स इसे इसलिए करते हैं क्योंकि हमेशा यह जानना जरूरी होता है कि कोई कार्य या प्रक्रिया किस समय हुई है। 

## कैसे करें?

PowerShell में वर्तमान तारीख प्राप्त करने के लिए कोड:
```PowerShell
Get-Date
```

जिसका आउटपुट ऐसा होगा:
```
गुरूवार, 20 जनवरी 2023 21:46:18
```

यदि आपको केवल तारीख चाहिए तो:
```PowerShell
Get-Date -Format "dd-MM-yyyy"
```

आउटपुट:
```
20-01-2023
```

## गहराई से जानकारी

PowerShell में `Get-Date` cmdlet वर्ष 2006 से है, जब PowerShell को पहली बार जारी किया गया था। यह .NET Framework के `System.DateTime` क्लास का उपयोग करता है। वैकल्पिक तरीकों में `[datetime]::Now` और `[datetime]::Today` जैसे डायरेक्ट .NET क्लास कॉल्स शामिल हैं, लेकिन `Get-Date` का उपयोग सबसे आम और सरल है।

विस्तार से समझें तो, `Get-Date` cmdlet कई तरह के प्रारूप (formats) और कस्टमीकृत आउटपुट को सपोर्ट करता है। यह भविष्य या अतीत की तारीखों को भी प्राप्त या मॉडिफाई करने में सहायक है, जैसे कि एक सप्ताह पहले या बाद की तारीख निकालना। 

## और देखो

- [Microsoft का आधिकारिक डॉक्यूमेंटेशन 'Get-Date'](https://docs.microsoft.com/powershell/module/microsoft.powershell.utility/get-date)
- [.NET के DateTime स्ट्रक्चर पर जानकारी](https://docs.microsoft.com/dotnet/api/system.datetime)
- [PowerShell प्रोग्रामिंग के बेसिक्स](https://mva.microsoft.com/search/SearchResults.aspx#!q=PowerShell&lang=1033)
