---
title:                "मानक त्रुटि के लिए लिखना"
aliases:
- /hi/elm/writing-to-standard-error/
date:                  2024-02-03T19:33:57.260848-07:00
model:                 gpt-4-0125-preview
simple_title:         "मानक त्रुटि के लिए लिखना"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

मानक त्रुटि (stderr) को लिखना मानक आउटपुट (stdout) के लिए मुख्य कार्यक्रम आउटपुट से अलग, त्रुटि संदेशों और निदान को पुनर्निर्देशित करने के बारे में है। प्रोग्रामर इसे त्रुटि हैंडलिंग और लॉगिंग को अधिक प्रबंधनीय बनाने के लिए करते हैं, विशेष रूप से ऐसे वातावरण में जहां आउटपुट विभेदन डिबगिंग और निगरानी के लिए महत्वपूर्ण होता है।

## कैसे करें:

एल्म मुख्य रूप से वेब विकास के लिए लक्षित होता है, जहाँ पारंपरिक कमांड-लाइन वातावरणों के समान तरीके से सीधे stderr पर लेखन की अवधारणा लागू नहीं होती। हालांकि, Node.js या इसी प्रकार के वातावरण में चलने वाले एल्म प्रोग्रामों के लिए, जावास्क्रिप्ट के साथ पोर्ट्स का उपयोग करके अंतरोप समान कार्यक्षमता प्राप्त करने का मुख्य दृष्टिकोण होता है। यहाँ आप इसे कैसे सेटअप कर सकते हैं:

एल्म कोड (`Main.elm`):
```elm
port module Main exposing (main)

import Browser

port errorOut : String -> Cmd msg

-- JS के लिए एक त्रुटि संदेश भेजने वाला डमी उदाहरण फ़ंक्शन
generateError : String -> Cmd msg
generateError message =
    errorOut message

main =
    generateError "यह stderr के लिए एक त्रुटि संदेश है"
```

जावास्क्रिप्ट अंतरोप (`index.js`):
```javascript
const { Elm } = require('./Main.elm');

var app = Elm.Main.init();

app.ports.errorOut.subscribe((message) => {
  console.error(message);
});
```

यह एल्म कोड एक पोर्ट `errorOut` परिभाषित करता है जो एल्म से जावास्क्रिप्ट के लिए संदेश भेजने की अनुमति देता है। तब जावास्क्रिप्ट कोड में, हम इस पोर्ट के माध्यम से भेजे गए संदेशों के लिए सुनते हैं और उन्हें `console.error()` का उपयोग करके stderr पर पुनर्निर्देशित करते हैं। इस तरह, आप जावास्क्रिप्ट के साथ एल्म की अंतरोप विशेषताओं का लाभ उठाकर उस वातावरण में प्रभावी रूप से stderr पर लिख सकते हैं जो इसका समर्थन करता है।

Node.js टर्मिनल में डेमो आउटपुट (जब `index.js` चलाया जाता है):
```
यह stderr के लिए एक त्रुटि संदेश है
```
