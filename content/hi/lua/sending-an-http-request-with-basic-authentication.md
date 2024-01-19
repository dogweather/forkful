---
title:                "बुनियादी प्रमाणीकरण के साथ एक हैचटीटीपी अनुरोध भेजना"
html_title:           "Lua: बुनियादी प्रमाणीकरण के साथ एक हैचटीटीपी अनुरोध भेजना"
simple_title:         "बुनियादी प्रमाणीकरण के साथ एक हैचटीटीपी अनुरोध भेजना"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Lua में HTTP अनुरोध भेजना: एक आसान उपाय

## What & Why?
HTTP अनुरोध भेजना क्या है? इसके माध्यम से हम एक वेब सर्वर से डेटा रिक्त कर सकते हैं और इसका उपयोग डेटा को प्राप्त करने या दर्ज करने के लिए किया जाता है। प्रोग्रामर्स इसे सुरक्षित रूप से करने के लिए करते हैं क्योंकि इससे सुनिश्चित होता है कि केवल प्राप्त करने या दर्ज करने योग्य उपयोगकर्ताओं को ही एक प्रकार का प्रवेश मिलेगा।

## How to:
```Lua
-- विशेष डेटा और मूल्य तय करें
local उर्ल = "https://example.com/login"
local उपयोगकर्तानाम = "एक्साम्पल यूजरनेम"
local पासवर्ड = "एक्साम्पल पासवर्ड"

-- HTTP अनुरोध भेजें
local अनुरोध = http.request {
    method = "POST",
    url = उर्ल,
    headers = {
        ["अनुशंसक"] = "एपिपीआई / 1.0"
    },
    auth = उपयोगकर्तानाम .. ":" .. पासवर्ड
}

-- सफल अनुरोध का प्रिंट करें
if अनुरोध.is_ok then
    print(अनुरोध.content)
else
--त्रुटि संदेश प्रिंट करें
    print(अनुरोध.reason)
end
```

उपरोक्त उदाहरण में, हमने HTTP अनुरोध भेजने के लिए `http.request` फ़ंक्शन का उपयोग किया है, जिसमें हमने अनुरोध के प्रकार, URL, अनुशंसक और भेजने के लिए उपयोगकर्तानाम और पासवर्ड जैसे विस्तृत डाक शीर्षक दर्ज किए हैं। उपरोक्त कोड उपयोगकर्तानाम और पासवर्ड के साथ बेसिक प्रमाणीकरण के साथ एक POST अनुरोध भेजेगा। अनुरोध सफल होने पर, हम अनुरोध के साथ डेटा प्राप्त कर सकते हैं और अन्यथा त्रुटि संदेश प्राप्त कर सकते हैं।

## Deep Dive:
बेसिक प्रमाणीकरण का उपयोग HTTP अनुरोधों को सुरक्षित बनाने के लिए किया जाता है। इसमें उपयोगकर्तानाम और पासवर्ड के साथ उपयोगकर्ता को पहचान देने के लिए एक बेसिक ऑथेंटिकेशन शीर्षक होता है। यह विशेष प्रकार का क्रिप्टोग्राफ़िक़ ऑल्गोरिदम नहीं है लेकिन कम सुरक्षित साइटों के लिए यह काफी अच्छा है।

कुछ अन्य विकल्पों में, जैसे डाईजेसेस्ट ऑथेंटिकेशन और OAuth, उपयोगकर्तानाम और पासवर्ड की जगह अन्य प्रक्रियाओं और टोकनों का उपयोग किया जाता है। इन विकल्पों का उपयोग सुरक्षा क