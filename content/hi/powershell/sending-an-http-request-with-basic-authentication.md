---
title:                "बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेजना"
date:                  2024-01-20T18:03:01.114697-07:00
model:                 gpt-4-1106-preview
simple_title:         "बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेजना"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/powershell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
HTTP अनुरोध के साथ बेसिक प्रमाणीकरण, या Basic Authentication, का उपयोग एक वेबसर्विस को यूजरनेम और पासवर्ड प्रदान करने के लिए किया जाता है, ताकि सुनिश्चित किया जा सके कि अनुरोध करने वाला व्यक्ति वही है जो वह दावा करता है। प्रोग्रामर्स इसे अक्सर तब करते हैं जब उन्हें किसी सुरक्षित एपीआई या सर्वर से डेटा एक्सेस करना होता है।

## कैसे करें:
```PowerShell
# यूजरनेम और पासवर्ड सेट करें
$user = "your_username"
$pass = "your_password"

# बेसिक ऑथ टोकन तैयार करें
$base64AuthInfo = [Convert]::ToBase64String([Text.Encoding]::ASCII.GetBytes(("{0}:{1}" -f $user, $pass)))

# HTTP हेडरों के साथ अनुरोध बनाएं
$headers = @{
    Authorization=("Basic {0}" -f $base64AuthInfo)
}

# HTTP GET अनुरोध करें
$response = Invoke-RestMethod -Uri "http://example.com/api/resource" -Method Get -Headers $headers

# अनुरोध का परिणाम दिखाएं
$response
```

इसका आउटपुट इस प्रकार होगा:
```PowerShell
# आउटपुट (यह आपके अनुरोध पर निर्भर करता है)
resourceData
```

## गहराई से जानकारी:
HTTP प्रमाणीकरण की अवधारणा 1990 के दशक से है, जब web कम्युनिकेशन को सुरक्षित बनाने की जरूरत थी। बेसिक प्रमाणीकरण यूजरनेम और पासवर्ड को Base64 एनकोडिंग में स्ट्रिंग के रूप में भेजता है। यह तरीका सुरक्षित नहीं है अगर SSL/TLS जैसी एनक्रिप्शन तकनीक का इस्तेमाल न किया जाए। 

विकल्प के रूप में, डिजिटल पहचान सत्यापन के लिए ओथ (OAuth), ओथ2 (OAuth2), या टोकन आधारित प्रमाणीकरण जैसे JWT (JSON Web Tokens) का उपयोग हो सकता है। 

इम्प्लीमेंटेशन में, पावरशेल इन्वोक-रेस्टमेथड कमांडलेट का उपयोग करते हुए अनुरोध भेजना आसान बनाता है, और यह JSON, XML, और अन्य प्रकार के डेटा को संभाल सकता है।    

## देखें भी:
- Microsoft का आधिकारिक पेज 'Invoke-RestMethod' पर: [Invoke-RestMethod](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod)
- Basic Authentication के बारे में अधिक जानकारी: [HTTP Authentication: Basic and Digest Access Authentication](https://tools.ietf.org/html/rfc2617)
- मजबूत प्रामाणीकरण के तरीकों पर गाइड: [OAuth 2.0](https://oauth.net/2/)
