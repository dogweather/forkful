---
title:                "स्ट्रिंग्स को जोड़ना"
html_title:           "Bash: स्ट्रिंग्स को जोड़ना"
simple_title:         "स्ट्रिंग्स को जोड़ना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)

स्ट्रिंग्स को जोड़ना (concatenating strings) मतलब होता है दो या अधिक स्ट्रिंग्स को एक साथ जोड़ना। प्रोग्रामर्स इसे टेक्स्ट को विशेष तरीके से फॉर्मैट या व्यवस्थित करने के लिए करते हैं।

## कैसे: (How to:)

Clojure में, `str` बिल्ट-इन फंक्शन स्ट्रिंग्स को कोन्कैटिनेट करने का तरीका है।

```clojure
(str "नमस्ते, " "दुनिया!")
; => "नमस्ते, दुनिया!"
```

`str` फंक्शन मल्टीपल आर्गुमेंट्स का समर्थन करता है:

```clojure
(str "नमस्ते, " "मैं " "Clojure " "सीख " "रहा " "हूँ।")
; => "नमस्ते, मैं Clojure सीख रहा हूँ।"
```

## गहराई में: (Deep Dive)

**ऐतिहासिक संदर्भ:** जब Clojure 2007 में शुरू हुआ, `str` फंक्शन पहले से ही उसमें शामिल था। 

**विकल्प:** जबकि `str` सबसे सामान्य तरीका है, जटिल टेक्स्ट माणिपुलेशन के लिए `clojure.string/join` में देखना चाहिए।

**क्रियान्वयन विवरण:** `str` फंक्शन अंतर्निहित रूप से Java का `StringBuilder` का उपयोग करता है।

## संबंधित स्रोतों में देखें: (See Also:)

- [Clojure Docs `str`](https://clojuredocs.org/clojure.core/str)
- [Clojure Docs `clojure.string/join`](https://clojuredocs.org/clojure.string/join)