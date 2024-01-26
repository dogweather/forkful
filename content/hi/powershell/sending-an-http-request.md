---
title:                "HTTP अनुरोध भेजना"
date:                  2024-01-20T18:00:22.385752-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP अनुरोध भेजना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
HTTP अनुरोध भेजना यानी आप इंटरनेट पर किसी वेबसर्वर से जानकारी मांग रहे होते हैं। प्रोग्रामर इसलिए ऐसा करते हैं क्योंकि वे डेटा पुनप्राप्त करना चाहते हैं या सर्वर को कुछ डेटा भेजना चाहते हैं।

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
