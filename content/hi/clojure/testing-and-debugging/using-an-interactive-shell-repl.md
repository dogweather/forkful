---
title:                "इंटरैक्टिव शेल (REPL) का उपयोग"
date:                  2024-01-26T04:13:53.423163-07:00
model:                 gpt-4-0125-preview
simple_title:         "इंटरैक्टिव शेल (REPL) का उपयोग"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
REPL, या Read-Eval-Print Loop, एक प्रोग्रामिंग वातावरण है, जिससे Clojure कोड को भाग-दर-भाग गतिशील रूप से परखा जा सकता है। कोडर्स इसका उपयोग तात्कालिक प्रतिक्रिया, चरणबद्ध विकास और बिना संकलन या पूरे परियोजना वातावरण की स्थापना के बिना त्वरित प्रयोगों के लिए करते हैं।

## कैसे करें:
REPL शुरू करने के लिए:

```Clojure
user=> (println "नमस्ते, REPL!")
नमस्ते, REPL!
nil
```

एक फ़ंक्शन को परिभाषित करें और उसका उपयोग करें:
```Clojure
user=> (defn greet [name] (str "नमस्ते, " name "!"))
#'user/greet
user=> (greet "Clojure प्रोग्रामर")
"नमस्ते, Clojure प्रोग्रामर!"
```

डाटा संरचनाओं के साथ प्रयोग करें:
```Clojure
user=> (def my-map {:a 1 :b 2})
#'user/my-map
user=> (assoc my-map :c 3)
{:a 1, :b 2, :c 3}
```

## गहराई में
REPL Lisp परिवार के अंतरक्रियाशील विकास दर्शन के लिए कुंजी है, और Clojure, एक आधुनिक Lisp बोली, इस उपकरण का शानदार उपयोग करती है। यह 1950 के दशक के अंत में पहले Lisp REPL तक जाती है। अन्य भाषाओं में विकल्प Python का इंटरप्रेटर और Node.js का कंसोल है, लेकिन Clojure का REPL पहले दर्जे का है और कार्यप्रवाह में अभिन्न है।

एक Clojure REPL सत्र को कमांड-लाइन, IDEs (जैसे IntelliJ के साथ Cursive, या Emacs के साथ CIDER), या ब्राउजर-आधारित उपकरणों जैसे कि Nightcode में विभिन्न पर्यावरणों में एकीकृत किया जा सकता है। गहरे अर्थों में, REPL विकासकर्ता को रन-टाइम पर भाषा की रचनाओं को संभालने और विभिन्न परिवर्तनों में स्थितियों को संचालित करने की शक्ति देता है, जो अक्सर प्रयोगात्मक प्रोग्रामिंग और अधिक मजबूत कोड की ओर ले जाता है।

`lein repl` या `clj` जैसे उपकरणों के साथ REPL की क्रियाकलापता चमकती है, जो निर्भरता प्रबंधन, विभिन्न प्लगइन्स, और परियोजना-विशिष्ट अनुकूलनों की अनुमति देते हैं, जिससे एक अधिक उत्पादक और लचीली विकास प्रक्रिया होती है।

## यह भी देखें
- REPL पर आधिकारिक Clojure वेबसाइट गाइड: https://clojure.org/guides/repl/introduction
- REPL-केन्द्रित विकास के बारे में रिच हिकी की बातचीत: https://www.youtube.com/watch?v=Qx0-pViyIDU
- व्यावहारिक Clojure: चरणबद्ध विकास के लिए REPL का उपयोग: http://practicalclj.blogspot.com/2009/10/using-clojure-repl.html