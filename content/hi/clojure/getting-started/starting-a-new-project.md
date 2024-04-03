---
date: 2024-01-20 18:03:52.492254-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Clojure\
  \ \u092A\u094D\u0930\u094B\u091C\u0947\u0915\u094D\u091F\u094D\u0938 \u0936\u0941\
  \u0930\u0942 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F Leiningen\
  \ \u092F\u093E Clojure CLI \u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\
  \u0932 \u0939\u094B\u0924\u093E \u0939\u0948\u0964 \u0906\u0907\u090F, Leiningen\
  \ \u0915\u093E \u090F\u0915 \u0909\u0926\u093E\u0939\u0930\u0923 \u0926\u0947\u0916\
  \u0924\u0947 \u0939\u0948\u0902\u0964 \u0928\u092F\u093E\u2026"
lastmod: '2024-03-13T22:44:51.664586-06:00'
model: gpt-4-1106-preview
summary: "Clojure \u092A\u094D\u0930\u094B\u091C\u0947\u0915\u094D\u091F\u094D\u0938\
  \ \u0936\u0941\u0930\u0942 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F\
  \ Leiningen \u092F\u093E Clojure CLI \u0915\u093E \u0907\u0938\u094D\u0924\u0947\
  \u092E\u093E\u0932 \u0939\u094B\u0924\u093E \u0939\u0948\u0964 \u0906\u0907\u090F\
  , Leiningen \u0915\u093E \u090F\u0915 \u0909\u0926\u093E\u0939\u0930\u0923 \u0926\
  \u0947\u0916\u0924\u0947 \u0939\u0948\u0902\u0964\n\n\u0928\u092F\u093E \u092A\u094D\
  \u0930\u094B\u091C\u0947\u0915\u094D\u091F \u092C\u0928\u093E\u0928\u0947 \u0915\
  \u0947 \u0932\u093F\u090F."
title: "\u0928\u0908 \u092A\u0930\u093F\u092F\u094B\u091C\u0928\u093E \u0936\u0941\
  \u0930\u0942 \u0915\u0930\u0928\u093E"
weight: 1
---

## How to: (कैसे करें:)
Clojure प्रोजेक्ट्स शुरू करने के लिए Leiningen या Clojure CLI का इस्तेमाल होता है। आइए, Leiningen का एक उदाहरण देखते हैं।

नया प्रोजेक्ट बनाने के लिए:
```Clojure
;; Leiningen स्थापित होना चाहिए
lein new app मेरा-प्रोजेक्ट
```

इस कमांड से `मेरा-प्रोजेक्ट` नामक एक नया प्रोजेक्ट बनेगा जिसमें आवश्यक फाइल्स और फोल्डर्स होंगे।

प्रोजेक्ट की संरचना कुछ इस तरह होगी:
```
मेरा-प्रोजेक्ट
├── project.clj
├── src
│   └── मेरा_प्रोजेक्ट
│       └── core.clj
└── test
    └── मेरा_प्रोजेक्ट
        └── core_test.clj
```

## Deep Dive (विस्तृत जानकारी)
Leiningen एक ऑटोमेशन टूल है जो 2010 में Phil Hagelberg द्वारा बनाया गया था और इसे Clojure कम्युनिटी में जल्दी ही स्वीकार किया गया। इसका मुख्य कार्य है प्रोजेक्ट मैनेजमेंट और बिल्ड ऑटोमेशन।

विकल्प के तौर पर Clojure CLI भी है, जो अधिक लाइटवेट है और Clojure 1.9 के बाद से उपलब्ध है। 

प्रोजेक्ट को शुरू करने से पहले `project.clj` या `deps.edn` फाइल में डिपेंडेंसीज और प्लगइन्स को कॉन्फिग करना होता है। ये प्लगइन्स डेवलपमेंट और डिप्लॉयमेंट को आसान बनाते हैं।

## See Also (और भी जानकारी)
- [Leiningen's Official Website](https://leiningen.org/)
- [Clojure's Official Reference](https://clojure.org/guides/getting_started)
- [Clojure CLI Tools](https://clojure.org/reference/deps_and_cli)
- [Clojure Build Tools](https://www.clojure-toolbox.com/)
