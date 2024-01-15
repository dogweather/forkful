---
title:                "बेसिक प्रमाणीकरण के साथ एक http अनुरोध भेजना"
html_title:           "Javascript: बेसिक प्रमाणीकरण के साथ एक http अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ एक http अनुरोध भेजना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्यों

क्या आपको पता है कि जब आप किसी वेब पेज पर लॉगिन करते हैं, तो आप कहीं न कहीं basic authentication के साथ एक HTTP अनुरोध भेजते हैं? बेसिक authentication, जो एक बेहतर authentication मेथड है, सुनिश्चित करता है कि केवल उपयोगकर्ता ही वह पेज देख सकते हैं जिनका अनुमति प्राप्त है। इस लेख में हम इस बेसिक authentication से समझैंगे।

## कैसे करें

आप इस *प्रोसेस* को स्टेप बाई स्टेप निम्नांकित तरीकों से समझ सकते हैं:

```Javascript
// आपको एक XMLHttpRequest object बनानी होगी
let xhr = new XMLHttpRequest();

// एक GET request का अनुरोध भेजें
xhr.open("GET", "https://www.example.com", true);

// इस request में basic authentication की ज़रूरत होती है, इसलिए आपको उपयोगकर्ता नाम और पासवर्ड भेजने होंगे
xhr.setRequestHeader("Authorization", "Basic " + btoa("username:password"));

// अनुरोध भेजें
xhr.send();

// उत्तर प्राप्त करें और उसे कंसोल में प्रिंट करें
xhr.onload = function() {
    console.log(xhr.responseText);
}
```

#### उत्पादन

जब आप यह कोड निष्क्रियता करते हैं, आप लॉग इन खाता के पृष्ठ पर रिडायरेक्ट होंगे, इसका अर्थ है आप सफलतापूरवक basic authentication द्वारा दस्तावेज़ मांग लिया। आप कॉन्सोल में इसके उत्तर को देख सकते हैं, जो वेब पेज की पूरी HTML कोड का उल्लेख करता है।

## गहराई में जाइये

जैसा कि हमने पहले कहा, basic authentication से हम सुनिश्चित करते हैं कि केवल उपयोगकर्ता ही वह पेज देख सकते हैं जिनका अनुमति प्राप्त है। लेकिन ये लॉग इन दस्तावेज़ का प्रारूप असल में क्या होता है?

बेसिक authentication बनाने के