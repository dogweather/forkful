---
title:                "Clojure: उपस्थितियों को अलग करना"
simple_title:         "उपस्थितियों को अलग करना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## क्यों

स्ट्रिंग के उपरस्थितियों को निकालने में जुड़ने का *क्यों* कारण है कि हमें हमारे कोड में प्रयोग कर सकते हैं।

## कैसे करें

```Clojure
(def str "मेरा नाम मॉली है")
(subs str 6)
```

```Clojure
मॉली
```

```Clojure
(def str "हिन्दी एक सुंदर भाषा है")
(subs str 12 17)
```

```Clojure
सुंदर
```

## गहराई में जाएं

स्ट्रिंग के उपरस्थितियों को निकालने के लिए, हम `subs` फ़ंक्शन का उपयोग कर सकते हैं। यह फ़ंक्शन दो आर्ग्यूमेंट्स लेता है - प्रारंभिक स्थान और समाप्त स्थान। हम इसे उपस्थितीयों की स्थान को स्थान के साथ निकालते हैं। इससे हम केवल उस प्रतिस्थापन उपरस्थिति को प्रिंट कर सकते हैं जिससे हमें स्थान मिला है।

## देखें भी

- [Clojure Cheat Sheet](https://clojure.org/guides/learn/syntax#_strings)
- [Official Clojure Documentation on `subs` function](https://clojuredocs.org/clojure.core/subs)
- [Clojure String Functions](https://www.tutorialspoint.com/clojure/clojure_strings.htm)