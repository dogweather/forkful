---
title:                "TOML के साथ काम करना"
date:                  2024-01-26T04:21:23.770235-07:00
model:                 gpt-4-0125-preview
simple_title:         "TOML के साथ काम करना"

category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/working-with-toml.md"
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
