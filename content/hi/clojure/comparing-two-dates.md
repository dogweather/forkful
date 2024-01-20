---
title:                "दो तारीखों की तुलना"
html_title:           "Elixir: दो तारीखों की तुलना"
simple_title:         "दो तारीखों की तुलना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/comparing-two-dates.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)

तारीखों की तुलना उन दिनांकों को तुलनात्मक गणना के द्वारा मुद्दे पर निर्धारित करने की प्रक्रिया है। प्रोग्रामर्स इसे कार्य प्रवाह में निर्णय लेने और डेटा की मान्यता सत्यापित करने के लिए करते हैं।

## कैसे (How to)

```Clojure
(require '[clj-time.core :as t])
(require '[clj-time.coerce :as c])

(def d1 (t/date-time 2020 12 31)) 
(def d2 (t/date-time 2021 1 1)) 

(println (< (c/to-long d1) (c/to-long d2))) ; will print true
```

इसमें, हमने पहली और दूसरी तारीख की तुलना कर रहे हैं। Output में "true" इसका परिणाम है, जिसका अर्थ है कि d1 वास्तव में d2 से पहले है। 

## गहराई में (Deep Dive)

Clojure में दिनांकों की तुलना विभिन्न पुस्तकालयों का उपयोग करती है, जैसे कि clj-time, जो Joda-Time पुस्तकालय पर निर्भर करती है। इसका इतिहास संज्ञान जावा की तारीख और समय APIs की कोमलताओं से उत्पन्न हुआ। 

विकल्प रूप में, आप java.time पुस्तकालय का उपयोग कर सकते हैं, जिसे Clojure के लिए java-time पुस्तकालय में लपेटा गया है। 

तारीखों की तुलना के लोजिक के विवरण के लिए, यहाँ उपयोग हो रही है `to-long` फ़ंक्शन जो कि epoch मिलिसेकंड्स में तारीख का प्रतिनिधित्व करता है, और उसके बाद सामान्य संख्या तुलना लोजिक का उपयोग करता है।

## और भी (See Also)

यहां कुछ अन्य स्रोत हैं जिन्हें आप देख सकते हैं: 

- [clj-time पुस्तकालय](https://github.com/clj-time/clj-time)
- [java-time पुस्तकालय](https://github.com/dm3/clojure.java-time)