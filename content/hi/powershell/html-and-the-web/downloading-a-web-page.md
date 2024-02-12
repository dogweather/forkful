---
title:                "वेब पेज डाउनलोड करना"
aliases:
- /hi/powershell/downloading-a-web-page/
date:                  2024-01-20T17:45:00.946322-07:00
model:                 gpt-4-1106-preview
simple_title:         "वेब पेज डाउनलोड करना"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

वेब पेज को डाउनलोड करना इसकी सामग्री को इंटरनेट से आपके सिस्टम पर सहेजना है। प्रोग्रामर्स इसे डेटा एकत्र करने, वेबसाइट स्थिति की निगरानी, और ऑटोमेशन के लिए करते हैं।

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
