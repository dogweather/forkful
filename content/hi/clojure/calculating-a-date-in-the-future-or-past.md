---
title:                "भविष्य या अतीत में तारीख की गणना"
html_title:           "Clojure: भविष्य या अतीत में तारीख की गणना"
simple_title:         "भविष्य या अतीत में तारीख की गणना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्या और ज़रूरत क्यों : भविष्य या विगत की तारीक हिसाब करना क्या है?

भविष्य या अतीत की तारीख का हिसाब लगाना मतलब एक विशेष तारीख से निश्चित समय इकाईयों (दिन, महीने आदि) को जोड़ना या घटाना। यह संगठनात्मक योजना, डेटाबेस क्वेरी, खेल कोडिंग के लिए बहुत महत्वपूर्ण हो सकता है। 

## कैसे :

Clojure में, ``clj-time``library का उपयोग करके भूतकाल या भविष्य की तारीख का हिसाब किया जा सकता है। 

```Clojure
(require '[clj-time.core :as t])
(require '[clj-time.periodic :as p])

(defn add-days [days]
  (t/plus (t/now) (t/days days)))

(defn sub-days [days]
  (t/minus (t/now) (t/days days)))

(println (add-days 10)) ; outputs: '2021-01-25T18:06:38.674Z' when today's date is '2021-01-15'
(println (sub-days 10)) ; outputs: '2021-01-05T18:06:38.674Z' when today's date is '2021-01-15'
```

## गहरा डूबना : 

Clojure programming language 2007 में Richie Hickey ने बनाई थी। इसका मुख्य उद्देश्य सरलता, शक्ति, और चुस्त स्थूल समझ का संगम है। 

अन्य तारीख/समय libraries जैसे की ``Joda-Time`` या मौजूदा जावा 8 डेटाटाइम API ऑफर alternative approaches हो सकते हैं। लेकिन clj-time library Clojure के साथ अच्छे integrate करती है और कम code में अधिक काम करने में मदद करती है। 

clj-time library, Joda-Time पर बनाई गई है जो Java में तारीख और समय के साथ काम करने वाली एक मान्यता प्राप्त API है। 

## अन्य जानकारियां :

- [क्लोजर का विकिपीडिया पृष्ठ](https://en.wikipedia.org/wiki/Clojure)
- [Clj-Time अधिकारी GitHub Repo](https://github.com/clj-time/clj-time)
- [An Introduction to Clojure](https://clojure.org/guides/getting_started)