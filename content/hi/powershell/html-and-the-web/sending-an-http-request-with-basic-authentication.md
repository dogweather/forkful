---
date: 2024-01-20 18:03:01.114697-07:00
description: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u0915\u0947 \u0938\u093E\u0925\
  \ \u092C\u0947\u0938\u093F\u0915 \u092A\u094D\u0930\u092E\u093E\u0923\u0940\u0915\
  \u0930\u0923, \u092F\u093E Basic Authentication, \u0915\u093E \u0909\u092A\u092F\
  \u094B\u0917 \u090F\u0915 \u0935\u0947\u092C\u0938\u0930\u094D\u0935\u093F\u0938\
  \ \u0915\u094B \u092F\u0942\u091C\u0930\u0928\u0947\u092E \u0914\u0930 \u092A\u093E\
  \u0938\u0935\u0930\u094D\u0921 \u092A\u094D\u0930\u0926\u093E\u0928 \u0915\u0930\
  \u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0915\u093F\u092F\u093E \u091C\u093E\
  \u0924\u093E \u0939\u0948, \u0924\u093E\u0915\u093F \u0938\u0941\u0928\u093F\u0936\
  \u094D\u091A\u093F\u0924\u2026"
lastmod: '2024-03-13T22:44:52.701922-06:00'
model: gpt-4-1106-preview
summary: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u0915\u0947 \u0938\u093E\u0925\
  \ \u092C\u0947\u0938\u093F\u0915 \u092A\u094D\u0930\u092E\u093E\u0923\u0940\u0915\
  \u0930\u0923, \u092F\u093E Basic Authentication, \u0915\u093E \u0909\u092A\u092F\
  \u094B\u0917 \u090F\u0915 \u0935\u0947\u092C\u0938\u0930\u094D\u0935\u093F\u0938\
  \ \u0915\u094B \u092F\u0942\u091C\u0930\u0928\u0947\u092E \u0914\u0930 \u092A\u093E\
  \u0938\u0935\u0930\u094D\u0921 \u092A\u094D\u0930\u0926\u093E\u0928 \u0915\u0930\
  \u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0915\u093F\u092F\u093E \u091C\u093E\
  \u0924\u093E \u0939\u0948, \u0924\u093E\u0915\u093F \u0938\u0941\u0928\u093F\u0936\
  \u094D\u091A\u093F\u0924 \u0915\u093F\u092F\u093E \u091C\u093E \u0938\u0915\u0947\
  \ \u0915\u093F \u0905\u0928\u0941\u0930\u094B\u0927 \u0915\u0930\u0928\u0947 \u0935\
  \u093E\u0932\u093E \u0935\u094D\u092F\u0915\u094D\u0924\u093F \u0935\u0939\u0940\
  \ \u0939\u0948 \u091C\u094B \u0935\u0939 \u0926\u093E\u0935\u093E \u0915\u0930\u0924\
  \u093E \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\
  \u0930\u094D\u0938 \u0907\u0938\u0947 \u0905\u0915\u094D\u0938\u0930 \u0924\u092C\
  \ \u0915\u0930\u0924\u0947 \u0939\u0948\u0902 \u091C\u092C \u0909\u0928\u094D\u0939\
  \u0947\u0902 \u0915\u093F\u0938\u0940 \u0938\u0941\u0930\u0915\u094D\u0937\u093F\
  \u0924 \u090F\u092A\u0940\u0906\u0908 \u092F\u093E \u0938\u0930\u094D\u0935\u0930\
  \ \u0938\u0947 \u0921\u0947\u091F\u093E \u090F\u0915\u094D\u0938\u0947\u0938 \u0915\
  \u0930\u0928\u093E \u0939\u094B\u0924\u093E \u0939\u0948\u0964."
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
