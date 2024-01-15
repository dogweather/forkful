---
title:                "स्ट्रिंग को निचे केस में रूपांतरण करना"
html_title:           "Javascript: स्ट्रिंग को निचे केस में रूपांतरण करना"
simple_title:         "स्ट्रिंग को निचे केस में रूपांतरण करना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्यों

किसी भी प्रोग्रामिंग भाषा में, एक स्ट्रिंग लोअर केस में बदलना एक आम समस्या है। यह कई प्रोग्रामिंग टास्क्स के लिए जरूरी हो सकता है। लोअर केस में स्ट्रिंग कनवर्ट करना आपको अपनी प्रोग्राम को अधिक डायनामिक बनाता है और स्ट्रिंग मैनिपुलेशन में कई समस्याओं को हल करने में मदद करता है।

## कैसे करें

```Javascript
let str = "Hello World";
console.log(str.toLowerCase()); // Output: hello world
```

यहां, हमने स्ट्रिंग `toLowerCase()` मेथड का उपयोग करके स्ट्रिंग `str` को लोअर केस में कनवर्ट किया है। इस मेथड के साथ आप भी निर्दिष्ट इंडेक्स से स्ट्रिंग के सभी अक्षरों को लोअर केस में बदल सकते हैं। उदाहरण के लिए, `str.toLowerCase(0, 5)` लिखने से स्ट्रिंग `Hello World` अब `hello World` हो जाएगा।

## गहराई में जाइये

 `.toLowerCase()` मेथड से आप अपनी स्ट्रिंग के सभी अक्षरों को लोअर केस में बदल सकते हैं। इसके साथ ही, यह सामान्य स्ट्रिंग ऑपरेशन के लिए नहीं होता है, लेकिन यह विभिन्न भाषाओं के आधार पर भी काम करता है। प्रोग्रामर के लिए यह एक उपयोगी टूल है जो उन्हें स्ट्रिंग मैनिपुलेशन को अधिक आसान और सुगम बनाता है।

## देखिए भी

[JavaScript String Methods](https://www.w3schools.com/jsref/jsref_obj_string.asp)
[String toLowerCase() Documentation](https://www.w3schools.com/jsref/jsref_tolowercase.asp)