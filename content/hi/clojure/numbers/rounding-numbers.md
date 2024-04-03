---
date: 2024-01-26 03:45:23.224908-07:00
description: "\u0915\u0948\u0938\u0947: Clojure \u092E\u0947\u0902, \u0939\u092E \u092E\
  \u0941\u0916\u094D\u092F \u0930\u0942\u092A \u0938\u0947 `Math/round`, `Math/floor`,\
  \ \u0914\u0930 `Math/ceil` \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\
  \u0924\u0947 \u0939\u0948\u0902."
lastmod: '2024-03-13T22:44:51.654565-06:00'
model: gpt-4-0125-preview
summary: "Clojure \u092E\u0947\u0902, \u0939\u092E \u092E\u0941\u0916\u094D\u092F\
  \ \u0930\u0942\u092A \u0938\u0947 `Math/round`, `Math/floor`, \u0914\u0930 `Math/ceil`\
  \ \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0924\u0947 \u0939\u0948\
  \u0902."
title: "\u0938\u0902\u0916\u094D\u092F\u093E\u0913\u0902 \u0915\u094B \u092A\u0942\
  \u0930\u094D\u0923\u093E\u0902\u0915 \u092C\u0928\u093E\u0928\u093E"
weight: 13
---

## कैसे:
Clojure में, हम मुख्य रूप से `Math/round`, `Math/floor`, और `Math/ceil` का उपयोग करते हैं:

```clojure
(Math/round 3.5) ; => 4
(Math/round 3.4) ; => 3

(Math/floor 3.7) ; => 3.0
(Math/ceil 3.2)  ; => 4.0
```

विशिष्ट दशमलव स्थानों के लिए, हम गुणा करते हैं, गोल करते हैं, और विभाजित करते हैं:

```clojure
(let [num 3.14159
      scale 1000]
  (/ (Math/round (* num scale)) scale)) ; => 3.142
```

## गहराई में डाइव
फैंसी प्रोग्रामिंग भाषाओं से पहले, गोलाई एक मैन्युअल प्रक्रिया थी, सोचें अबेकस या कागज। प्रोग्रामिंग में, यह फ्लोटिंग-पॉइंट सटीकता सीमाओं के कारण संख्या प्रस्तुति के लिए महत्वपूर्ण है।

गोलाई के वैकल्पिक तरीके में सटीकता नियंत्रण के लिए `BigDecimal` क्लास का उपयोग या उन्नत गणितीय कार्यों के लिए `clojure.math.numeric-tower` जैसी लाइब्रेरीज शामिल हैं। Clojure का `Math/round` जावा के `Math.round`, `Math/floor`, और `Math/ceil` फंक्शन्स पर आधारित है, जिसका अर्थ है कि यह वही फ्लोट और डबल विशेषताएँ अपनाता है।

कार्यान्वयन के लिहाज से, Clojure में गोल करते समय, याद रखें कि यह स्वचालित रूप से डबल सटीकता का उपयोग करता है जब दशमलव से निपटता है। गोलाई त्रुटियों के प्रति सावधान रहें!

## देखें भी
- Clojure Math API: [https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/*math-context*](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/*math-context*)
- जावा मैथ API: [https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html](https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html)
- फ्लोटिंग-पॉइंट सटीकता को समझना: [https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)
