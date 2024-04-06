---
date: 2024-01-20 18:03:01.114697-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: \u0907\u0938\u0915\
  \u093E \u0906\u0909\u091F\u092A\u0941\u091F \u0907\u0938 \u092A\u094D\u0930\u0915\
  \u093E\u0930 \u0939\u094B\u0917\u093E."
lastmod: '2024-04-05T21:53:54.664979-06:00'
model: gpt-4-1106-preview
summary: "\u0907\u0938\u0915\u093E \u0906\u0909\u091F\u092A\u0941\u091F \u0907\u0938\
  \ \u092A\u094D\u0930\u0915\u093E\u0930 \u0939\u094B\u0917\u093E."
title: "\u092C\u0947\u0938\u093F\u0915 \u092A\u094D\u0930\u092E\u093E\u0923\u0940\u0915\
  \u0930\u0923 \u0915\u0947 \u0938\u093E\u0925 HTTP \u0905\u0928\u0941\u0930\u094B\
  \u0927 \u092D\u0947\u091C\u0928\u093E"
weight: 45
---

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
