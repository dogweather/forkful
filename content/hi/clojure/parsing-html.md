---
title:                "HTML पार्स करना"
html_title:           "C++: HTML पार्स करना"
simple_title:         "HTML पार्स करना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/parsing-html.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
HTML पार्सिंग, वेब पेज के विश्लेषण की प्रक्रिया है जिसमें टैग्स, एट्रिब्यूट्स और डेटा को निकाला जाता है। प्रोग्रामर्स इसे वेब स्क्रेपिंग, सोशल मीडिया साइट्स की मॉनिटरिंग, और स्वचालित टेस्टिंग आदि के लिए करते हैं।

## कैसे पार्सिंग करें:
Clojure में Enlive लाइब्ररी का उपयोग करके HTML पर्सिंग कर सकते हैं। इसके लिए पहले के लाइब्ररी को कॉन्फ़िगर करें:

```Clojure
(require '[net.cgrand.enlive-html :as html])
```

अब, आप HTML फ़ाइल को लोड कर सकते हैं:

```Clojure
(defn load-html [filename]
  (html/html-resource (java.io.File. filename)))
```

और इसे पार्स कर सकते हैं:

```Clojure
(defn parse-html [html-file]
  (-> (load-html html-file)
      (html/select [:body])
      first
      :content
      str))
```

उदाहरण कार्यान्वयन पर चलाने पर, आपको HTML के कंटेंट का आउटपुट मिलेगा।

## गहरी पड़ताल:
HTML पार्सिंग की आवश्यकता सामान्यतः तब होती है जब आपको वेब पेज से डेटा निकालना हो। Clojure में Enlive नामक लाइब्ररी का उपयोग करके इसे आसानी से किया जा सकता है। हालांकि, इतना ही नहीं, ऐसे भी कई प्रघ्यापन हैं जिनमें आप JSoup, Hiccup जैसी अन्य लाइब्ररीज का उपयोग कर सकते हैं।

## अधिक देखें:
HTML पार्सिंग के बारे में और अधिक जानकारी के लिए, यह लिंक्स देखें:
2. [Enlive डॉक्स](https://github.com/cgrand/enlive)
3. [HTML पार्सिंग का प्रलेखन](https://developer.mozilla.org/en-US/docs/Web/HTML)