---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:54.165220-07:00
description: "\u0915\u0948\u0938\u0947: Clojure \u092E\u0947\u0902, \u0906\u092A `*err*`\
  \ \u0938\u094D\u091F\u094D\u0930\u0940\u092E \u0915\u093E \u0909\u092A\u092F\u094B\
  \u0917 \u0915\u0930\u0915\u0947 stderr \u092E\u0947\u0902 \u0932\u093F\u0916 \u0938\
  \u0915\u0924\u0947 \u0939\u0948\u0902\u0964 \u092F\u0939\u093E\u0901 \u090F\u0915\
  \ \u092E\u0942\u0932 \u0909\u0926\u093E\u0939\u0930\u0923 \u0939\u0948."
lastmod: '2024-03-13T22:44:51.691638-06:00'
model: gpt-4-0125-preview
summary: "Clojure \u092E\u0947\u0902, \u0906\u092A `*err*` \u0938\u094D\u091F\u094D\
  \u0930\u0940\u092E \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\
  \u0947 stderr \u092E\u0947\u0902 \u0932\u093F\u0916 \u0938\u0915\u0924\u0947 \u0939\
  \u0948\u0902\u0964 \u092F\u0939\u093E\u0901 \u090F\u0915 \u092E\u0942\u0932 \u0909\
  \u0926\u093E\u0939\u0930\u0923 \u0939\u0948."
title: "\u092E\u093E\u0928\u0915 \u0924\u094D\u0930\u0941\u091F\u093F \u0915\u0947\
  \ \u0932\u093F\u090F \u0932\u093F\u0916\u0928\u093E"
weight: 25
---

## कैसे:
Clojure में, आप `*err*` स्ट्रीम का उपयोग करके stderr में लिख सकते हैं। यहाँ एक मूल उदाहरण है:

```clojure
(.write *err* "यह एक त्रुटि संदेश है.\n")
```

ध्यान दें कि संदेश लिखने के बाद, आपको स्ट्रीम को फ़्लश करना चाहिए ताकि संदेश तुरंत आउटपुट हो:

```clojure
(flush)
```

stderr पर नमूना आउटपुट:
```
यह एक त्रुटि संदेश है.
```

यदि आप अपवादों को संभाल रहे हैं, तो आप stderr पर स्टैक ट्रेसेस को प्रिंट करना चाह सकते हैं। इसके लिए `printStackTrace` का प्रयोग करें:

```clojure
(try
  ;; कोड जो एक अपवाद फेंक सकता है
  (/ 1 0)
  (catch Exception e
    (.printStackTrace e *err*)))
```

अधिक संरचित त्रुटि लॉगिंग के लिए, `timbre` जैसी तृतीय-पक्ष लाइब्रेरीज को stderr में लॉग करने के लिए कॉन्फिगर किया जा सकता है। यहाँ एक मूल सेटअप और उपयोग है:

सबसे पहले, आपकी निर्भरताओं में `timbre` जोड़ें। फिर इसे stderr का उपयोग करने के लिए कॉन्फिगर करें:

```clojure
(require '[taoensso.timbre :as timbre])

(timbre/set-config! [:appenders :standard-out :enabled?] false) ;; stdout लॉगिंग को अक्षम करें
(timbre/set-config! [:appenders :spit :enabled?] false) ;; फाइल लॉगिंग को अक्षम करें
(timbre/set-config! [:appenders :stderr :min-level] :error) ;; त्रुटियों के लिए stderr को सक्षम करें

(timbre/error "आपके अनुरोध को संसाधित करते समय एक त्रुटि हुई है।")
```

इससे त्रुटि-स्तर के संदेश stderr पर निर्देशित होंगे, जिससे वे मानक अनुप्रयोग आउटपुट से अलग हो जाएंगे।
