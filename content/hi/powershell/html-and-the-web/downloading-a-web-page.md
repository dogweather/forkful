---
date: 2024-01-20 17:45:00.946322-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: #."
lastmod: '2024-03-13T22:44:52.700329-06:00'
model: gpt-4-1106-preview
summary: '#.'
title: "\u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\u094B\
  \u0921 \u0915\u0930\u0928\u093E"
weight: 42
---

## कैसे करें:


### सामान्य वेब पेज को डाउनलोड करना:
```PowerShell
$response = Invoke-WebRequest -Uri 'http://example.com'
$response.Content | Out-File 'example.html'
```

### सैंपल आउटपुट:
```PowerShell
dir example.html
```

```
    Directory: C:\Users\YourUsername

Mode                LastWriteTime         Length Name
----                -------------         ------ ----
-a----        1/2/2023   3:15 PM          15423 example.html
```

## गहराई से जानकारी:
वेब पेज को डाउनलोड करने का काम पहले ज्यादातर `wget` और `curl` जैसे टूल्स से होता था। PowerShell में `Invoke-WebRequest` और `Invoke-RestMethod` cmdlets ने इस काम को सरल बनाया है। इसके अल्टरनेटिव के रूप में .NET क्लासेज `WebClient` और `HttpClient` हैं, लेकिन PowerShell Cmdlets अधिक सहज हैं। डाउनलोड किए गए पेज में स्क्रिप्ट और डायनामिक कंटेंट नहीं चलेगा, क्योंकि वे ब्राउज़र के जावास्क्रिप्ट इंजन द्वारा संचालित होते हैं।

## संबंधित सूत्र:
- Microsoft PowerShell Documentation: [Invoke-WebRequest](https://docs.microsoft.com/en-us/powershell/module/Microsoft.PowerShell.Utility/Invoke-WebRequest)
- .NET API Documentation: [HttpClient Class](https://docs.microsoft.com/en-us/dotnet/api/system.net.http.httpclient)

इन लिंक्स पर जाकर आप PowerShell और .NET API की और जानकारियाँ प्राप्त कर सकते हैं।
