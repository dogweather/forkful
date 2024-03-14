---
date: 2024-01-26 04:21:23.770235-07:00
description: "TOML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\
  \u0928\u0947 \u0915\u093E \u092E\u0924\u0932\u092C \u0939\u0948 \u0915\u093F \u0906\
  \u092A \"Tom's Obvious, Minimal Language\" \u092A\u094D\u0930\u093E\u0930\u0942\u092A\
  \ \u092E\u0947\u0902 \u0921\u0947\u091F\u093E \u0938\u0902\u092D\u093E\u0932 \u0930\
  \u0939\u0947 \u0939\u0948\u0902, \u091C\u094B \u0907\u0938\u0915\u0940 \u0938\u0930\
  \u0932 \u092A\u0922\u093C\u0928\u0947 \u092F\u094B\u0917\u094D\u092F\u0924\u093E\
  \ \u0915\u0947 \u0915\u093E\u0930\u0923 \u0915\u0949\u0928\u094D\u095E\u093F\u0917\
  \ \u092B\u093E\u0907\u0932\u094B\u0902\u2026"
lastmod: '2024-03-13T22:44:51.703493-06:00'
model: gpt-4-0125-preview
summary: "TOML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u0947 \u0915\u093E \u092E\u0924\u0932\u092C \u0939\u0948 \u0915\u093F \u0906\u092A\
  \ \"Tom's Obvious, Minimal Language\" \u092A\u094D\u0930\u093E\u0930\u0942\u092A\
  \ \u092E\u0947\u0902 \u0921\u0947\u091F\u093E \u0938\u0902\u092D\u093E\u0932 \u0930\
  \u0939\u0947 \u0939\u0948\u0902, \u091C\u094B \u0907\u0938\u0915\u0940 \u0938\u0930\
  \u0932 \u092A\u0922\u093C\u0928\u0947 \u092F\u094B\u0917\u094D\u092F\u0924\u093E\
  \ \u0915\u0947 \u0915\u093E\u0930\u0923 \u0915\u0949\u0928\u094D\u095E\u093F\u0917\
  \ \u092B\u093E\u0907\u0932\u094B\u0902\u2026"
title: "TOML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?
TOML के साथ काम करने का मतलब है कि आप "Tom's Obvious, Minimal Language" प्रारूप में डेटा संभाल रहे हैं, जो इसकी सरल पढ़ने योग्यता के कारण कॉन्फ़िग फाइलों के लिए लोकप्रिय है। प्रोग्रामर्स इसका उपयोग सरल कॉन्फ़िगुरेशन प्रबंधन के लिए करते हैं जो मानव-हितकारी सिंटैक्स के साथ बॉक्स से बाहर सही प्रकार से काम करता है।

## कैसे करें:
Clojure में TOML के साथ काम करने के लिए, आपको `clj-toml` जैसी लाइब्रेरी की आवश्यकता है। पहले, इसे अपनी `deps.edn` में जोड़ें:

```clojure
{:deps {clj-toml {:mvn/version "0.5.0"}}}
```

फिर कुछ TOML पार्स करें:

```clojure
(require '[clj-toml.core :as toml])

(def config-str "title = 'TOML Example'")

(def parsed-config (toml/parse-string config-str))

;; पार्स किये गए TOML से टाइटल प्राप्त करें
(println (:title parsed-config)) ;; आउटपुट: TOML Example
```

TOML बनाने के लिए:

```clojure
(def data {:title "TOML Example"})

(println (toml/generate-string data))
;; आउटपुट: title = "TOML Example"
```

## गहराई में उतरें
TOML का निर्माण 2013 के आस-पास Tom Preston-Werner, GitHub के सह-संस्थापक द्वारा, YAML और JSON के मुकाबले कॉन्फ़िग फाइलों के लिए एक सरल विकल्प के रूप में किया गया था। इसका उद्देश्य स्पष्टता है और यह मानवों द्वारा अतिरिक्त उपकरणों के बिना पढ़े जाने योग्य एक स्पेक होने की इच्छा रखती है।

जहां JSON अक्सर APIs और वेब ऐप्स के लिए इस्तेमाल किया जाता है, और YAML संदर्भ और स्क्रिप्ट क्षमताओं के साथ जटिल हो सकता है, TOML सरल, टेबल-आधारित संरचनाओं पर ध्यान केंद्रित के साथ खड़ा है। यह सादगी इसे विशेष रूप से Rust समुदाय और अन्य आधुनिक भाषा वातावरणों में लोकप्रिय बनाती है।

सादगी और व्यावहारिकता पर ध्यान केंद्रित करते हुए, Clojure, TOML के साथ कॉन्फ़िग के लिए अच्छी तरह से जुड़ता है। `clj-toml` या वैकल्पिक लाइब्रेरियां गैप को पाटती हैं। वे TOML के स्थिर डेटा को Clojure के गतिशील, कार्यात्मक विश्व में अनुवादित करती हैं।

## इसे भी देखें
- TOML की GitHub रेपो: [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- `clj-toml` on Clojars: [clojars.org/clj-toml](https://clojars.org/clj-toml)
- Clojure डॉक्स: [clojure.org](https://clojure.org/guides/getting_started)
- `clj-toml` का परिचय: [github.com/lantiga/clj-toml](https://github.com/lantiga/clj-toml)
