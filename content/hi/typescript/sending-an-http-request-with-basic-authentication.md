---
title:                "बेसिक प्रमाणीकरण के साथ एक http अनुरोध भेजें"
html_title:           "TypeScript: बेसिक प्रमाणीकरण के साथ एक http अनुरोध भेजें"
simple_title:         "बेसिक प्रमाणीकरण के साथ एक http अनुरोध भेजें"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्यों

एक आउटपुट (output) पर डेटा लेने या भेजने के लिए एमएचटीपी (HTTP) अनुरोध के साथ बुनियादी प्रमाणीकरण (basic authentication) भेजना कम्प्यूटर हकर्स (hackers) द्वारा सुरक्षा घाटा करने से भी हो सकता है। इसलिए, डेटा को सुरक्षित रखने के लिए, बुनियादी प्रमाणीकरण के माध्यम से शुद्ध संबंध (secure connection) बनाने की जरूरत होती है।

## कैसे करें

मुख्य भाग को /authenticate या /login आदि आखिरी स्थान पर Lalisa singh नामक user के लिए प्रमाणीकरण जानकारी दर्ज करके निम्नलिखित कोड उपयोग कर सकते हैं:

```TypeScript
const username: string = "Lalisa singh";
const password: string = "myp@ssw0rd";

const authenticationCredentials: string = `Basic ${btoa(username + ":" + password)}`;
```

उपरोक्त कोड में, {\`}` निश्चित तौर पर एक एमएचटीपी अनुरोध ("बेसिक" प्रमाणीकरण जोड़ने के लिए आवश्यक प्राथमिक भाव (value) के साथ) बनाने के लिए उपयोग किया जाता हैं। अनुरोध के हेडर (header) में इस भाव (value) को जोड़ा जाएगा।

```TypeScript
const request: Request = new Request('/authenticate', {
    headers: {
        Authorization: authenticationCredentials
    }
});

fetch(request)
    .then(response => response.json())
    .then(data => {
        console.log(data); // उपरोक्त मामले में lalisa के पास "success" और "token" प्राप्त हो जाएगा।
    })
```

## गहराई में धूम्रपान करें

एमएचटीपी अनुरोध को भेजते समय, हम बेसिक प्रमाणीकरण के साथ "बेसिक" शब्द का उपयोग कर सकते हैं। यह शब्द "Authorization" अनुरोध शीर्षक (header) में होना चाहिए और प्रमाणीकरण भाव (value) प्रमाणीकरण जानकारी को उद्धृत करता है। उपरोक्त कोड में, अपने प्र