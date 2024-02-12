---
title:                "नई परियोजना शुरू करना"
aliases:
- /hi/clojure/starting-a-new-project/
date:                  2024-01-20T18:03:52.492254-07:00
model:                 gpt-4-1106-preview
simple_title:         "नई परियोजना शुरू करना"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
एक नई प्रोजेक्ट शुरू करना मतलब है नए आइडियाज पर कार्य करना, नई समस्याओं का समाधान खोजना। प्रोग्रामर्स इसके लिए उत्सुक रहते हैं क्योंकि नए प्रोजेक्ट क्रिएटिविटी को बढ़ाते हैं और कोडिंग स्किल्स को निखारते हैं।

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
