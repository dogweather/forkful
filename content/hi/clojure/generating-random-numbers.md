---
title:    "Clojure: बेतरतीब संख्याओं का उत्पादन"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## क्यों

क्या आपको कभी यह सवाल पूछा गया है कि कोडिंग में रैन्डम नंबर क्यों उपयोगी होते हैं? रैन्डम नंबर हमारी डेटा एकीकरण कैपेबिलिटी को बढ़ाने में मदद करते हैं और अनुरोधों को अपने अभियानों से आवश्यकतानुसार विभिन्न गतिविधियों को प्रदान करते हैं।

## कैसे करें

आप Clojure में रैन्डम नंबर कैसे बना सकते हैं? इसके दो सरल तरीके हैं। पहला तरीका है ```(rand)```, जो आपको 0 से 1 तक के बीच किसी भी अनिश्चित दशमलव नंबर को देगा। दूसरा तरीका है ```(rand-int n)```, जो आपको 0 से n-1 तक के किसी भी पूर्णांक को देगा। नीचे कुछ उदाहरण दिए गए हैं:

```Clojure
(rand) ; 0.380126873612
(rand-int 10) ; 5
(rand-int 50) ; 23
```

## गहराई में जाएं

रैन्डम नंबर जेनरेशन के पीछे रचनात्मकता की वैज्ञानिक गहराई क्या है? रैन्डम नंबर जेनरेशन अल्गोरिदम में संगठित कुछ मानक विशेषताओं का उपयोग करता है जो अन्य प्रकार के नंबर जेनरेटर्स से अलग होते हैं। यह उम्मीद करना कितना मुमकिन होगा कि अगला नंबर क्या होगा और सुनिश्चित रूप से हर एक संभावनाओं के साथ उपभोग के लिए नंबर का चयन करना आप के लिए बड़ी उच्चालित कार्य होता है।

## देखिए भी

- [Clojure डॉक्यूमेंटेशन](https://clojure.org/api/cheatsheet)
- [कागज़ी नोट](https://www.math.utah.edu/~gere/HistTech/LCGpapers/Schrage_on_potential.pdf)