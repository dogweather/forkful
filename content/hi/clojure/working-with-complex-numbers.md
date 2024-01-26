---
title:                "जटिल संख्याओं के साथ काम करना"
date:                  2024-01-26T04:39:34.087708-07:00
model:                 gpt-4-0125-preview
simple_title:         "जटिल संख्याओं के साथ काम करना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
जटिल संख्याएँ वास्तविक संख्याओं को एक अतिरिक्त भाग, काल्पनिक इकाई 'i' के साथ विस्तारित करती हैं। प्रोग्रामर उन्हें विभिन्न क्षेत्रों में उपयोग करते हैं, जिनमें सिग्नल प्रोसेसिंग, विद्युतचुंबकीय सिद्धांत और फ्रैक्टल शामिल हैं, जहां एक नकारात्मक संख्या के वर्गमूल के साथ गणना आम है।

## कैसे करें:
Clojure `clojure.lang.Numbers` उपयोगिता क्लास के माध्यम से जटिल संख्याओं के लिए निर्मित समर्थन प्रदान करता है। `complex` का उपयोग करके जटिल संख्याएँ बनाएँ और अंकगणितीय क्रियाएँ करें।

```clojure
;; जटिल संख्याएँ बनाना
(def a (clojure.lang.Numbers/complex 3 4))  ; 3 + 4i
(def b (clojure.lang.Numbers/complex 1 -1)) ; 1 - i

;; जोड़
(+ a b) ;=> #object[clojure.lang.Numbers.Complex 0x5c6cfe9 "4 + 3i"]

;; घटाव
(- a b) ;=> #object[clojure.lang.Numbers.Complex 0x5e51118 "2 + 5i"]

;; गुणा
(* a b) ;=> #object[clojure.lang.Numbers.Complex 0x6ec3f0df "7 + i"]

;; भाग
(/ a b) ;=> #object[clojure.lang.Numbers.Complex 0x5db0cd10 "3.5 + 3.5i"]

;; संयुग्म
(.conjugate a) ;=> #object[clojure.lang.Numbers.Complex 0x47c6e076 "3 - 4i"]
```

## गहराई से 
जटिल संख्याओं को 18वीं सदी में गणितज्ञों जैसे कि गॉस और ऑयलर ने औपचारिक रूप दिया था। हालांकि शुरुआत में संदेह के साथ मिला, वे आधुनिक विज्ञान और इंजीनियरिंग में महत्वपूर्ण बन गए हैं। Clojure में कुछ भाषाओं की तरह एक मूल जटिल संख्या प्रकार नहीं है (उदाहरण के लिए, Python), लेकिन शामिल Java इंटरॉप `clojure.lang.Numbers` क्लास के माध्यम से आवश्यक संचालन संभाल सकता है।

Java का `java.lang.Complex` एक मजबूत विकल्प है, जो अधिक सुविधाएँ और संभावित ऑप्टिमाइज़ेशन प्रदान करता है। Clojure की होस्ट इंटरऑपरेबिलिटी Java लाइब्रेरीज के साथ काम करना आसान बनाता है।

आधार के नीचे, जटिल संख्या अंकगणितीय वास्तविक और काल्पनिक भागों को जोड़ने और गुणा करने में शामिल होता है, मुख्य नियम के साथ कि `i^2 = -1` होता है। जटिल संख्या विभाजन अधिक जटिल हो सकता है, आम तौर पर जटिल संख्याओं द्वारा विभाजन से बचने के लिए संयुग्म की आवश्यकता होती है।

## देखें भी
- ClojureDocs, त्वरित संदर्भ के लिए: https://clojuredocs.org/
- `java.lang.Complex` के लिए Java API: https://docs.oracle.com/javase/8/docs/api/java/lang/Complex.html
- गणितीय रूप से जिज्ञासु के लिए जटिल संख्याओं पर विकिपीडिया पेज: https://en.wikipedia.org/wiki/Complex_number