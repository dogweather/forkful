---
title:                "यादृच्छिक संख्याओं का निर्माण"
html_title:           "Clojure: यादृच्छिक संख्याओं का निर्माण"
simple_title:         "यादृच्छिक संख्याओं का निर्माण"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

रैंडम नंबर समन्वय है जो कंप्यूटर कोड के द्वारा पैदा की जाती है, जिसमें कोई विस्तृत नियम नहीं होता है। प्रोग्रामर्स इसे गेम डेवलपमेंट, क्रिप्टोग्राफी, या मोंटे कार्लो सिम्युलेशन्स के लिए उपयोग करते हैं।

## कैसे:

Clojure में, आप निम्नलिखित कोड का उपयोग करके एक रैंडम नंबर उत्पन्न कर सकते हैं:

```Clojure
(rand) ; मुद्रण एक रैंडम नंबर बीच 0 (सम्मिलित) और 1 (विलक्षण).
```

और अगर आपको एक रैंडम पूर्णांक चाहिए, तो आप `rand-int` का उपयोग कर सकते हैं। नीचे दिए गए कोड से, मुद्रण एक रैंडम पूरी संख्या बीच 0 (सम्मिलित) और 100 (विलक्षण).

```Clojure
(rand-int 100)
```
## गहरी गोता दुखी:

रैंडम नंबर उत्पन्न करने का विचार कंप्यूटर विज्ञान का महत्वपूर्ण हिस्सा है। सरल नकली यादृच्छिकता के लिए, Clojure जावा के `java.util.Random` कक्ष का उपयोग करता है। यदि आपको अधिक शक्तिशाली और सुरक्षित रैंडमनेस की आवश्यकता हो तो, Clojure बिना किसी संशोधन के जावा के `SecureRandom` तर्क संगठन का उपयोग करता है।

## देखने के लिए भी:

1. [Clojure की आधिकारिक ट्यूटोरियल](https://clojure.org/guides/getting_started)
2. [जावा नामक स्थल ju.Random के बारे में प्रलेखन](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
3. [जावा नामक स्थल SecureRandom के बारे में प्रलेखन](https://docs.oracle.com/javase/8/docs/api/java/security/SecureRandom.html)