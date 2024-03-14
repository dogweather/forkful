---
date: 2024-01-20 18:00:22.385752-07:00
description: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u093E\
  \ \u092F\u093E\u0928\u0940 \u0906\u092A \u0907\u0902\u091F\u0930\u0928\u0947\u091F\
  \ \u092A\u0930 \u0915\u093F\u0938\u0940 \u0935\u0947\u092C\u0938\u0930\u094D\u0935\
  \u0930 \u0938\u0947 \u091C\u093E\u0928\u0915\u093E\u0930\u0940 \u092E\u093E\u0902\
  \u0917 \u0930\u0939\u0947 \u0939\u094B\u0924\u0947 \u0939\u0948\u0902\u0964 \u092A\
  \u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930 \u0907\u0938\u0932\u093F\u090F\
  \ \u0910\u0938\u093E \u0915\u0930\u0924\u0947 \u0939\u0948\u0902 \u0915\u094D\u092F\
  \u094B\u0902\u0915\u093F \u0935\u0947 \u0921\u0947\u091F\u093E \u092A\u0941\u0928\
  \u092A\u094D\u0930\u093E\u092A\u094D\u0924 \u0915\u0930\u0928\u093E \u091A\u093E\
  \u0939\u0924\u0947 \u0939\u0948\u0902\u2026"
lastmod: '2024-03-13T22:44:52.696994-06:00'
model: gpt-4-1106-preview
summary: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u093E\
  \ \u092F\u093E\u0928\u0940 \u0906\u092A \u0907\u0902\u091F\u0930\u0928\u0947\u091F\
  \ \u092A\u0930 \u0915\u093F\u0938\u0940 \u0935\u0947\u092C\u0938\u0930\u094D\u0935\
  \u0930 \u0938\u0947 \u091C\u093E\u0928\u0915\u093E\u0930\u0940 \u092E\u093E\u0902\
  \u0917 \u0930\u0939\u0947 \u0939\u094B\u0924\u0947 \u0939\u0948\u0902\u0964 \u092A\
  \u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930 \u0907\u0938\u0932\u093F\u090F\
  \ \u0910\u0938\u093E \u0915\u0930\u0924\u0947 \u0939\u0948\u0902 \u0915\u094D\u092F\
  \u094B\u0902\u0915\u093F \u0935\u0947 \u0921\u0947\u091F\u093E \u092A\u0941\u0928\
  \u092A\u094D\u0930\u093E\u092A\u094D\u0924 \u0915\u0930\u0928\u093E \u091A\u093E\
  \u0939\u0924\u0947 \u0939\u0948\u0902\u2026"
title: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u093E"
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
