---
title:                "Elm: पैटर्न से मेल खाते अक्षरों को हटाना"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# क्यों

किसी ने जब आप EDM प्रोग्रामिंग कर उसके लिए कॉड को हटाने में रूचि लेने के लिए कहे तो वह आपको समझने की आवश्यकता हो सकती है की वह कैसे काम करता है तथ्यों पर

# कैसे

```Elm
import Text.Regex

string = "Mera Naam Elm Hai"
newString = Regex.replace (Regex.Regex "a") (always "") string
```

उपरोक्त कोड उदाहरण से दिए गए कोड में आप देख सकते हैं की `Mera Naam Elm Hai`को `Mera Nm Elm Hi` में बदल दिया गया है। यहां होने वाला है कि एक नया स्ट्रिंग जैसे की `newString` में आप देख सकते हैं

# गहराई

जब आप EDM में किसी पैटर्न के साथ मिलती है तो आप या नहीं है-कुछ से सौंटे है। यह एक आसानी है. `Regex`मॉड्युल, और इस मॉड्युल में उपलब्ध विधि और उपक्रमों का उपयोग है। आप किसी भी प्रकार के पैटर्न, जैसे की इस्तेमाल करते हैं`Regex.Regex` और दूसरी विधियों जैसे की `Regex.replace` आप संबंधित कोड सेक्शन क्लास में देख सकते हैं.

# उसे भी देखें

[Kaiv Chhadva - Elm Language](https://medium.com/@kaichhadva/elm-language-5e1b2d433637) 
[Elm in Production](https://www.humblebits.com/elm-in-production/) 
[Official Elm Website](https://elm-lang.org/)