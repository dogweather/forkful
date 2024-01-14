---
title:                "Javascript: एचटीटीपी अनुरोध भेजना"
simple_title:         "एचटीटीपी अनुरोध भेजना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## क्यों

एचटीटीपी अनुरोध भेजने में कार्य क्यों भी कभी भी हासिल किया गया हो सकता है।

## कैसे करें

```Javascript
// असिंक एचटीटीपी अनुरोध भेजने का उदाहरण
var request = new XMLHttpRequest();
request.open('GET', 'https://example.com/data');
request.onreadystatechange = function () {
    if (this.readyState === 4) { // स्टेट 4 अपने काम को पूरा करने के बाद प्राप्त होता है
        if (this.status === 200) { // सही स्थिति में सफल होने पर आवेदन
            console.log('डाटा प्राप्त किया गया: ', this.responseText);
        } else { // समस्याओं के साथ दुबारा से सही स्थिति पाने के लिए फिर से प्रयास करें
            console.log('अनुरोध विफल हुआ');
        }
    }
};
request.send(); // अनुरोध भेजें
```

ऊपर दिए गए कोड ब्लॉक में हम एक अनुरोध को बनाने का उदाहरण दिया है और उसे भेजने और प्राप्त करने के लिए कैसे उपयोग करने के लिए केंद्रीकृत करने का तरीका दिखाया है। यह तरीका सिंक होता है, यानि अनुरोध भेजने के बाद हम जानकारी को प्राप्त करने के लिए उपयोग कर सकते हैं।

```Javascript
// सिंक एचटीटीपी अनुरोध भेजने का उदाहरण
var request = new XMLHttpRequest();
request.open('GET', 'https://example.com/data', false); // सिंक अनुरोध के लिए "false" अनुमति नहीं होती
request.send();
if (request.status === 200) {
    console.log('डाटा प्राप्त किया गया: ', request.responseText);
} else {
    console.log('अनुरोध विफल हुआ');
}
```

ऊपर दिए गए कोड में, हम इस्तेमाल करते हैं `false` सिंक अनुमति जो उपयुक्त है जब हम एचटीटीपी अनुरोध भेजते हैं और एक उत्तर प्राप्त करते हैं। तो हम डाटा को स्थानीय पर्यावरण में सीधे प