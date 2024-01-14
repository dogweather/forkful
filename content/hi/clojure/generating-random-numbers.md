---
title:                "Clojure: यादृच्छिक संख्याएं उत्पन्न करना"
programming_language: "Clojure"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## क्यों

क्या आप यह जानते हैं कि प्रोग्रामिंग में नंबर्स को कैसे रैंडमली जेनरेट किया जाता है? ऐसा करने से प्रोग्राम को और द्यामिक बनाने में मदद मिलती है और बहुत सारे अनुकूल स्किल्स भी विकसित होते हैं।

## कैसे

```Clojure
(import 'java.util.Random)

(def rand (java.util.Random.)) ; न संख्या का सीधा प्रतिनिधित्व

(defn random-number [min max]
  (+ min (.nextInt rand (- max min))))

(println (random-number 1 10)) ; प्रत्येक बार कोई अन्य नंबर गेनरेट होता है।
```

## गहराई में डूबोआओ

रैंडम नंबर्स जेनरेट करने के लिए, कई अलग-अलग तकनीकों का उपयोग किया जा सकता है। इनमें से कुछ का उल्लेख निम्नानुसार है:

- लीनियर कंग्रुइअल जेनरेटर: इसका उपयोग दुनियाभर में आज भी बहुत होता है। यह प्रगतिशील तकनीकों से अनुमति देता है जिनसे हमें आसानी से रैंडम नंबर जनरेट करने में मदद मिलती है।

- मरसेन्न ट्रायोंगल जेनरेटर: यह एक और बहुत अन्यंता जेनरेटर है जो गणित की दुनिया में वक्त के साथ ग़ुलाम हो गया है! हालाँकि यह उपयोग में काफी शक्तिशाली है।

## देखिए भी

- [Clojure.org](https://clojure.org/)
- [Clojure का VIM](https://github.com/guns/vim-clojure-static)
- [लिस्प - एक अद्भुत प्रोग्रामिंग भाषा](https://lisp-lang.org/)