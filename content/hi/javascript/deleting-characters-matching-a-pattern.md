---
title:                "एक पैटर्न से मेल खाते अक्षरों को हटाना।"
html_title:           "Javascript: एक पैटर्न से मेल खाते अक्षरों को हटाना।"
simple_title:         "एक पैटर्न से मेल खाते अक्षरों को हटाना।"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## क्यों

कभी-कभी, हमें अपने दस्तावेज़ों में से एक विशिष्ट पैटर्न के मैच होने वाले वर्णों को हटाने की आवश्यकता हो सकती है। इसके लिए, दस्तावेज़ को चालू स्थिति में रखने के लिए और इसे साफ और साफ करने के लिए वर्णों को हटाना अहम रूप से उपयोगी हो सकता है।

## कैसे करे

```Javascript
// धारा में विशिष्ट मैत्रों को हटाने के लिए एक RegEx का उपयोग करें
let str = "यह ईसा अमर्त्य है। वह कभी नहीं मरेगा।";
let newStr = str.replace(/[^a-zA-Z\s]/g, "");

// आउटपुट: "यह ईसा अमर्त्य है वह कभी नहीं मरेगा"
```

इस उदाहरण में, हमने दस्तावेज़ के शुरुआती ज़बानी अक्षरों को बचाने के लिए कोड के माध्यम से RegEx का उपयोग किया है। यदि आप विशिष्ट वर्णों को दर्ज करना चाहते हैं, तो आप उन्हें भी हटा सकते हैं। इसके अलावा, आप स्पेस और अन्य अल्फान्यूमेरिक वर्णों को भी हटा सकते हैं।

## गहराई में जाने

कोडिंग नियमों को अनुसरण करने के लिए विशिष्ट वर्णों को हटाने के लिए एक RegEx का उपयोग करने में हमेशा सहायक हो सकता है। हम इस तकनीक को विभिन्न प्रकार के डेटा प्रसंस्करण और फोर्मेटिंग कार्यों में भी लागू कर सकते हैं।

## इसे देखें

[MDN गाइड: RegEx](https://developer.mozilla.org/hi/docs/Web/JavaScript/Guide/Regular_Expressions)

[GeeksforGeeks: RegEx और इसका उपयोग](https://www.geeksforgeeks.org/javascript-regex-examples/)

[Hackerrank: RegEx की समस्याओं का समाधान