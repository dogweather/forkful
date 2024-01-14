---
title:    "Clojure: स्ट्रिंग को कैपिटलाइज़ करना"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

#
## क्यों

कोई आदमी अपनी स्ट्रिंग को कैपिटलाइज करना क्यों पसंद करेगा? सही तरीके से लिखा हुआ टेक्स्ट हमारे कोड को प्रोफेशनल लगाता है और हमारे प्रोग्राम को अधिक पढ़ने वाले बनाता है। इससे आसानी से बड़ी स्ट्रिंग को भी संबोधित करने में मदद मिलती है।

## कैसे करे

```
Clojure
(defn capitalize [string] ; कोड ब्लॉक शुरू होता है
     (str/capitalize string)) ; दी गयी स्ट्रिंग वेरिएबल को कैपिटलाइज करता है
; ("hello world") का आउटपुट "Hello world" होगा
```

## गहराई में

स्ट्रिंग को कैपिटलाइज करने का और भी एक तरीका है। ```clojure.string``` library में इसके लिए एक फ़ंक्शन है जिसका नाम ``camel-case`` है। यह एक दिए गए टेक्स्ट को कैमलकेस में बदलेगा जो मतलब होता है कि हर पहले शब्द का पहला अक्षर कैपिटल होगा। उदाहरण के लिए, "hello world" को "HelloWorld" में बदल देगा।

## देखें भी

- [Clojure Official Documentation on Strings](https://clojuredocs.org/clojure.core/capitalize)
- [Clojure Docs for the String Library](https://clojuredocs.org/clojure.string)