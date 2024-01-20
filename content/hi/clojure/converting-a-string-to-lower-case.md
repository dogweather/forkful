---
title:                "एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
html_title:           "Kotlin: एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
simple_title:         "एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

एक string को lower case में convert करना मतलब उसमें मौजूद सभी characters को छोटे हर्फों में बदल देना। इसका इस्तेमाल programmers मुख्यतः दो strings की comparison के लिए करते हैं। इससे case-sensitivity का issue नहीं रहता। 

## कैसे करें:

Clojure में हमें सिर्फ `clojure.string/lower-case` का इस्तेमाल करना होगा। यह function एक string को lower case में convert कर देता है। 

```Clojure
(require '[clojure.string :as str])

(str/lower-case "Hello, World!")
```

इसकी output इस प्रकार होगी: 

```Clojure
"hello, world!"
```

## गहराई में:

### ऐतिहासिक संदर्भ:
Clojure का `clojure.string/lower-case` function Java के `toLowerCase()` method पर आधारित है। यह उसे अधिक फ़ंक्शनल और आसान बनाता है।

### विकल्प:
यदि आपके पास Clojure 1.1 या उससे पुराना version है, तो आप इसे Java interop के माध्यम से कर सकते हैं:

```Clojure
(.toLowerCase "Hello, World!")
```

इसका output भी same होगा।

### आंतरिक कार्य:
`clojure.string/lower-case` तय निर्देशों का पालन करता है जैसे दिए गए हैं `Character.toLowerCase` method में। यह Unicode characters को भी समर्थन करता है। 

## भी देखें:

- Clojure का [String Library](https://clojuredocs.org/clojure.string) 
- Java का [toLowerCase() Method](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#toLowerCase()) 
- Unicode के [Lowercase Conversion Details](https://www.unicode.org/versions/Unicode13.0.0/ch03.pdf#G33992) (PDF link)