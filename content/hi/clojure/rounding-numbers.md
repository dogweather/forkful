---
title:                "संख्याओं को पूर्णांक बनाना"
aliases:
- hi/clojure/rounding-numbers.md
date:                  2024-01-26T03:45:23.224908-07:00
model:                 gpt-4-0125-preview
simple_title:         "संख्याओं को पूर्णांक बनाना"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/rounding-numbers.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
संख्याओं को गोल करना यानी किसी संख्या को निकटतम पूर्णांक या किसी निश्चित दशमलव सटीकता तक समायोजित करना। हम संख्याओं को उन्हें मानव पठनीयता के लिए सरल बनाने, गणनात्मक भार को कम करने, या विशिष्ट संख्यात्मक आवश्यकताओं को पूरा करने के लिए गोल करते हैं।

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
