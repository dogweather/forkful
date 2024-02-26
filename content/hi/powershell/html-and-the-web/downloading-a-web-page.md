---
date: 2024-01-20 17:45:00.946322-07:00
description: "\u0935\u0947\u092C \u092A\u0947\u091C \u0915\u094B \u0921\u093E\u0909\
  \u0928\u0932\u094B\u0921 \u0915\u0930\u0928\u093E \u0907\u0938\u0915\u0940 \u0938\
  \u093E\u092E\u0917\u094D\u0930\u0940 \u0915\u094B \u0907\u0902\u091F\u0930\u0928\
  \u0947\u091F \u0938\u0947 \u0906\u092A\u0915\u0947 \u0938\u093F\u0938\u094D\u091F\
  \u092E \u092A\u0930 \u0938\u0939\u0947\u091C\u0928\u093E \u0939\u0948\u0964 \u092A\
  \u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0907\u0938\u0947\
  \ \u0921\u0947\u091F\u093E \u090F\u0915\u0924\u094D\u0930 \u0915\u0930\u0928\u0947\
  , \u0935\u0947\u092C\u0938\u093E\u0907\u091F \u0938\u094D\u0925\u093F\u0924\u093F\
  \ \u0915\u0940 \u0928\u093F\u0917\u0930\u093E\u0928\u0940, \u0914\u0930 \u0911\u091F\
  \u094B\u092E\u0947\u0936\u0928 \u0915\u0947\u2026"
lastmod: '2024-02-25T18:49:49.900003-07:00'
model: gpt-4-1106-preview
summary: "\u0935\u0947\u092C \u092A\u0947\u091C \u0915\u094B \u0921\u093E\u0909\u0928\
  \u0932\u094B\u0921 \u0915\u0930\u0928\u093E \u0907\u0938\u0915\u0940 \u0938\u093E\
  \u092E\u0917\u094D\u0930\u0940 \u0915\u094B \u0907\u0902\u091F\u0930\u0928\u0947\
  \u091F \u0938\u0947 \u0906\u092A\u0915\u0947 \u0938\u093F\u0938\u094D\u091F\u092E\
  \ \u092A\u0930 \u0938\u0939\u0947\u091C\u0928\u093E \u0939\u0948\u0964 \u092A\u094D\
  \u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0907\u0938\u0947\
  \ \u0921\u0947\u091F\u093E \u090F\u0915\u0924\u094D\u0930 \u0915\u0930\u0928\u0947\
  , \u0935\u0947\u092C\u0938\u093E\u0907\u091F \u0938\u094D\u0925\u093F\u0924\u093F\
  \ \u0915\u0940 \u0928\u093F\u0917\u0930\u093E\u0928\u0940, \u0914\u0930 \u0911\u091F\
  \u094B\u092E\u0947\u0936\u0928 \u0915\u0947\u2026"
title: "\u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\u094B\
  \u0921 \u0915\u0930\u0928\u093E"
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
