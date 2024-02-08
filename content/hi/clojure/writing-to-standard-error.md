---
title:                "मानक त्रुटि के लिए लिखना"
aliases:
- hi/clojure/writing-to-standard-error.md
date:                  2024-02-03T19:33:54.165220-07:00
model:                 gpt-4-0125-preview
simple_title:         "मानक त्रुटि के लिए लिखना"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
मानक त्रुटि (stderr) में लिखने का अर्थ है त्रुटि संदेशों और नैदानिक जानकारियों को stderr स्ट्रीम में निर्देशित करना, मानक आउटपुट (stdout) से अलग। प्रोग्रामर्स नियमित कार्यक्रम आउटपुट और त्रुटि संदेशों को अलग करने के लिए ऐसा करते हैं, जिससे डिबगिंग और लॉगिंग अधिक प्रभावी हो सके।

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
