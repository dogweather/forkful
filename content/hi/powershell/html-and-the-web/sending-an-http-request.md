---
date: 2024-01-20 18:00:22.385752-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) PowerShell\
  \ \u092E\u0947\u0902 `Invoke-RestMethod` \u092F\u093E `Invoke-WebRequest` cmdlet\
  \ \u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0915\u0930\u0915\
  \u0947 HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u093E \u091C\
  \u093E\u0924\u093E \u0939\u0948\u0964 GET \u0905\u0928\u0941\u0930\u094B\u0927 \u0915\
  \u0947 \u0932\u093F\u090F."
lastmod: '2024-04-05T21:53:54.661157-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) PowerShell \u092E\u0947\
  \u0902 `Invoke-RestMethod` \u092F\u093E `Invoke-WebRequest` cmdlet \u0915\u093E\
  \ \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0915\u0930\u0915\u0947 HTTP\
  \ \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u093E \u091C\u093E\u0924\
  \u093E \u0939\u0948\u0964 GET \u0905\u0928\u0941\u0930\u094B\u0927 \u0915\u0947\
  \ \u0932\u093F\u090F."
title: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u093E"
weight: 44
---

## How to: (कैसे करें:)
PowerShell में `Invoke-RestMethod` या `Invoke-WebRequest` cmdlet का इस्तेमाल करके HTTP अनुरोध भेजा जाता है। 

GET अनुरोध के लिए:
```PowerShell
$response = Invoke-RestMethod -Uri 'http://example.com/api/data' -Method Get
$response
```

POST अनुरोध के लिए, JSON डेटा के साथ:
```PowerShell
$body = @{
    'key1' = 'value1'
    'key2' = 'value2'
} | ConvertTo-Json

$response = Invoke-RestMethod -Uri 'http://example.com/api/data' -Method Post -Body $body -ContentType 'application/json'
$response
```

उदाहरण आउटपुट:
```
{
    "responseKey1": "responseValue1",
    "responseKey2": "responseValue2"
}
```

## Deep Dive (गहराई से जानकारी):
HTTP अनुरोध 1990 के दशक में आरम्भ हुआ जब वर्ल्ड वाइड वेब बनाया गया। PowerShell `Invoke-RestMethod` और `Invoke-WebRequest` cmdlets आजकल के स्क्रिप्ट्स में RESTful APIs से जुड़ने का आम तरीका हैं।

वैकल्पिक तरीके में `curl` या `.NET` क्लासेस जैसे `HttpClient` आते हैं, लेकिन PowerShell cmdlets आसानी से समझने योग्य और इस्तेमाल करने में सुविधाजनक होते हैं। 

`Invoke-RestMethod` ज्यादातर JSON या XML डेटा फॉर्मेट के लिए है, जबकि `Invoke-WebRequest` एक ज्यादा सामान्य HTTP अनुरोध cmdlet है जिसमें पूरे HTTP प्रतिक्रिया ऑब्जेक्ट तक एक्सेस मिलती है।

## See Also (और देखें):
- [Invoke-RestMethod documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod)
- [Invoke-WebRequest documentation](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest)
- [Understanding REST APIs](https://www.redhat.com/en/topics/api/what-is-a-rest-api)
- [About HTTP Requests](https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods)
