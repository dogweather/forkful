---
title:                "कमांड लाइन तर्कों को पढ़ना"
html_title:           "Kotlin: कमांड लाइन तर्कों को पढ़ना"
simple_title:         "कमांड लाइन तर्कों को पढ़ना"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

आदेश लाइन तर्क या Command Line Arguments उन परामीतियों को कहा जाता है जो हमें एक कमांड के प्रारंभ में देने होते हैं। ये स्वतंत्रता देते हैं की प्रोग्राम को किस तरह चलाना है। उदाहरण स्वरुप, हम किसी फ़ाइल के नाम को एक कमांड लाइन आर्गुमेंट के रूप में पास कर सकते हैं।

## कैसे:

माफ़ कीजिये, लेकिन Elm में कमांड लाइन आर्गुमेंट्स का समर्थन डिफ़ॉल्ट रूप से उपलब्ध नहीं है। लेकिन, आप निम्न तरीके का उपयोग कर सकते हैं जिसमे आप एक Node.js script बनाते हैं जो आपके Elm प्रोग्राम को कमांड लाइन आर्गुमेंट्स पास करेगा:

```Elm
var { app } = require('./elm/main.js');
var input = process.argv[2];

app.Main.init({flags: input});
```
उपर्युक्त कोड एक Node.js script हैं जो कमांड लाइन आर्गुमेंट्स को एक Elm प्रोग्राम में पास करने के लिए हैं।

## गहराई में:

तब जब Elm बना, उसके लक्ष्य में वेब विकास को सशुल्क और सुविधाजनन बनाना था, इसलिए इसे एक प्रमुख ब्राउज़र-आधारित भाषा के रूप में विकसित किया गया। इसका मतलब यह है कि कुछ डिफ़ॉल्ट उपयोग के मामले (जैसे कि command line arguments का उपयोग करना) समर्थित नहीं हो सकते हैं। फिर भी, आपको कमांड लाइन आर्गुमेंट्स के साथ काम करने के लिए कोई "पुल" (जैसे Node.js) का उपयोग करना होगा। 

## और भी देखें:

1. Elm में Flags का उपयोग करें: [https://guide.elm-lang.org/interop/flags.html](https://guide.elm-lang.org/interop/flags.html)