---
title:                "Clojure: स्ट्रिंग हैंड्लिंग का जोड़ना"
simple_title:         "स्ट्रिंग हैंड्लिंग का जोड़ना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## क्यों

कोण्केटनेटिंग स्ट्रिंग्स चलाने का कारण है कि यह आपको अपने कोड में स्ट्रिंग्स को स्थानों और प्रोग्रामों के साथ मिश्रित करने की सुविधा प्रदान करता है। 

## कैसे करें

```Clojure
; उदाहरण 1
(println (str "मैं" " " "एक" " " "ऑनलाइन" " " "प्रोग्रामर" " " "हूँ"))

; उत्पादन:
मैं एक ऑनलाइन प्रोग्रामर हूँ

; उदाहरण 2
(def first-name "राहुल")
(def last-name "गुप्ता")
(println (str "मेरा" " " "नाम" " " first-name " " last-name " " "है"))

; उत्पादन:
मेरा नाम राहुल गुप्ता है
```

## विस्तार से जानिए

कोण्केटनेटिंग स्ट्रिंग्स का उपयोग किसी भी प्रकार की स्ट्रिंग्स को एक साथ जोड़ने के लिए किया जाता है। इससे हम प्रोग्रामिंग के साथ साथ अपने स्ट्रिंग्स को भी एक साथ लिख सकते हैं जो कि एक जिनिस जोड़ने में आसानी प्रदान करता है। इसमें हम विभिन्न तरीकों से स्ट्रिंग्स को जोड़ सकते हैं जैसे कि `str` फ़ंक्शन या `concat` फ़ंक्शन।

## देखें भी

- [Clojure डॉक्युमेंटेशन](https://clojure.org/guides/learn/syntax)
- [Java और Clojure स्ट्रिंग्स का तुलनात्मक विश्लेषण](https://cemerick.com/posts/2010/08/06/string-perversions-in-the-java-clojure-venn/)
- [Clojure स्ट्रिंग्स पर हिंदी ट्यूटोरियल](https://www.develves.net/blogs/asd/2016-05-26Clojure-concatenating-strings/)