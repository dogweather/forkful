---
title:    "Javascript: स्ट्रिंग को कैपिटलाइज करना"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्यों

अगर आप JavaScript में कोई स्ट्रिंग कैपिटलाइज करने की जरूरत होती है, तो आप सभी को यह पता होना चाहिए कि आप ऐसा क्यों करना चाहते हैं। इस ब्लॉग पोस्ट में हम आपको बताएंगे कि स्ट्रिंग को कैपिटलाइज कैसे किया जाता है और इसके पीछे की विस्तार से जानकारी देंगे।

## कैसे करें

जब हमें किसी स्ट्रिंग को कैपिटलाइज करने की जरूरत होती है, तो हम `toUpperCase()` फ़ंक्शन का इस्तेमाल करते हैं। इसका उपयोग करके हम दिए गए स्ट्रिंग को बड़े अक्षर में ट्रांसफ़र कर सकते हैं। नीचे एक उदाहरण दिया गया है:

```Javascript
var str = "hello world";
console.log(str.toUpperCase());
// Output: HELLO WORLD
```

इसी तरह, हम `toLowerCase()` फ़ंक्शन का इस्तेमाल करके स्ट्रिंग को छोटे अक्षर में ट्रांसफ़र कर सकते हैं। नीचे एक और उदाहरण दिया गया है:

```Javascript
var str = "HELLO WORLD";
console.log(str.toLowerCase());
// Output: hello world
```

## गहराई में जाएं

जब हम `toUpperCase()` या `toLowerCase()` फ़ंक्शन का इस्तेमाल करते हैं, तो इस फ़ंक्शन का पूरा स्ट्रिंग पर एक्सेस होता है और इसके बाद उस फ़ंक्शन में दिए गए अक्षरों को बदल देता है। अगर आप चाहें तो आप खुद भी एक `toUpperCase()` या `toLowerCase()` फ़ंक्शन बना सकते हैं। इसके लिए आपको स्ट्रिंग को लूप करना होगा और हर अक्षर को सटीक ढंग से बदलना होगा। हम यहां एक उदाहरण देखेंगे:

```Javascript
function toUpperCase(str) {
    var result = "";
    for (var i = 0; i < str.length; i++) {
        var char = str[i];
        if (/[a-z]/