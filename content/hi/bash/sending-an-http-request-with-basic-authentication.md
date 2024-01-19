---
title:                "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
html_title:           "C#: बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
HTTP अनुरोध के साथ मूल ग्राहक पहुँच भेजना क्या है? इससे प्रोग्रामर्स अपने सर्वर को अपनी पहचान सत्यापित करने में सहायता करते हैं। यदि आपके पास मान्य प्रमाणीकरण जानकारी नहीं है, तो सर्वर अनुरोध अस्वीकार कर सकता है। 

## कैसे करें:
Bash कोड प्रयोग करके आप सरलता से HTTP अनुरोध के साथ मूल प्रमाणीकरण भेज सकते हैं। 

```Bash
#!/bin/bash

USERNAME="username"
PASSWORD="password"

URL="http://your-webpage.com"
AUTH="Authorization: Basic $(echo -n "$USERNAME:$PASSWORD" | base64)"

curl -H "$AUTH" "$URL"
```
उपरोक्त ब्लॉक का उपयोग करें और आवश्यकतानुसार यूजरनेम, पासवर्ड और URL बदलें। Output:

```Bash
{
  "authenticated": true,
  "user": "username"
}
```
## गहराई में: 
HTTP Basic Authentication का उपयोग पहली बार RFC 2617 (जून 1999) में पेश किया गया था। ऑल्टरनेटिव्स में OAuth, Digest Access Authentication, और SCRAM-SHA-1 शामिल हैं। इसपर अमल करते समय, प्रमाणीकरण की जानकारी को Base64 encoding का उपयोग करके कोडित किया जाता है। हालांकि, ध्यान दें कि Base64 encoding आसानी से decode की जा सकती है, इसलिए HTTPS तकनीक का उपयोग करना सलाह देते हैं।

## देखें भी: 
यदि आपको इस विषय पर और अधिक जानकारी चाहिए, तो निम्नलिखित लिंक्स देखें। 

- [HTTP authentication - MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [HTTP Basic Auth - HTTP | MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme)