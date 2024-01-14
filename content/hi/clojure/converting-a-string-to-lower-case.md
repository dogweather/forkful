---
title:                "Clojure: शब्द स्तर को निचे लाना"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्यों
क्या आपको कभी कभी स्ट्रिंग को लोअर केस में कन्वर्ट करने की जरूरत होती है? स्ट्रिंग ऑपरेशन में लोअर केस का उपयोग अक्सर टेक्स्ट प्रसंस्करण और डेटा मैनिपुलेशन के लिए किया जाता है।

## कैसे करें
आइए देखते हैं कि कैसे हम Clojure में स्ट्रिंग को लोअर केस में कन्वर्ट कर सकते हैं। नीचे दिए गए कोड ब्लॉक में Clojure स्क्रिप्ट को देखें:

```Clojure
(def text "HELLO WORLD")
(.toLowerCase text)
```

इसका उत्पादन निम्न रूप में होगा:

```Clojure
hello world
```

## गहराई तक
Clojure में स्ट्रिंग को लोअर केस में कन्वर्ट करने के लिए, हम `toLowerCase` के साथ एक स्ट्रिंग ऑब्जेक्ट का उपयोग कर सकते हैं। यह फंक्शन स्ट्रिंग को दो श्रृंखलाओं में अलग करता है - एक श्रृंखला उपर केस और दूसरी श्रृंखला लोअर केस। स्ट्रिंग की गहराई तक होने से, हम स्ट्रिंग को सुविधाजनक तरीके से ड्यानामिकली अपडेट और प्रोसेस कर सकते हैं।

## और देखें
"स्ट्रिंग के सभी अक्षरों को यूनिकोड मानों में कन्वर्ट करने का तरीका" - https://www.tutorialspoint.com/clojure/clojure_string.htm

"क्लोजर में स्ट्रिंग से अंक स्ट्रिप करने का तरीका" - https://clojure.org/guides/java_interop

"प्रोरेसिंग में स्ट्रिंग प्रसंस्करण के आगे कैसे बढ़ें" - https://technologyconversations.com/2014/01/20/clojure-tutorial-processing-data-with-clojure/