---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:55.252740-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: \u092B\u093E\u0907\
  \u0932 \u092E\u0947\u0902 \u091F\u0947\u0915\u094D\u0938\u094D\u091F \u0932\u093F\
  \u0916\u0928\u0947 \u0915\u093E `spit` \u092B\u093C\u0902\u0915\u094D\u0936\u0928\
  \ \u0938\u092C\u0938\u0947 \u0938\u0930\u0932 \u0924\u0930\u0940\u0915\u093E \u0939\
  \u0948\u0964 \u0907\u0938\u092E\u0947\u0902 \u0926\u094B \u0924\u0930\u094D\u0915\
  \ \u0939\u094B\u0924\u0947 \u0939\u0948\u0902: \u092B\u093E\u0907\u0932 \u092A\u0925\
  \ \u0914\u0930 \u0932\u093F\u0916\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0938\
  \u094D\u091F\u094D\u0930\u093F\u0902\u0917\u0964 \u092F\u0926\u093F \u092B\u093E\
  \u0907\u0932 \u092E\u094C\u091C\u0942\u0926 \u0928\u0939\u0940\u0902 \u0939\u0948\
  ,\u2026"
lastmod: '2024-03-13T22:44:51.694953-06:00'
model: gpt-4-0125-preview
summary: "\u092B\u093E\u0907\u0932 \u092E\u0947\u0902 \u091F\u0947\u0915\u094D\u0938\
  \u094D\u091F \u0932\u093F\u0916\u0928\u0947 \u0915\u093E `spit` \u092B\u093C\u0902\
  \u0915\u094D\u0936\u0928 \u0938\u092C\u0938\u0947 \u0938\u0930\u0932 \u0924\u0930\
  \u0940\u0915\u093E \u0939\u0948\u0964 \u0907\u0938\u092E\u0947\u0902 \u0926\u094B\
  \ \u0924\u0930\u094D\u0915 \u0939\u094B\u0924\u0947 \u0939\u0948\u0902."
title: "\u090F\u0915 \u091F\u0947\u0915\u094D\u0938\u094D\u091F \u092B\u093C\u093E\
  \u0907\u0932 \u0932\u093F\u0916\u0928\u093E"
weight: 24
---

## कैसे करें:


### Clojure के निर्मित कार्यों का उपयोग करके फाइल में टेक्स्ट लिखना
फाइल में टेक्स्ट लिखने का `spit` फ़ंक्शन सबसे सरल तरीका है। इसमें दो तर्क होते हैं: फाइल पथ और लिखने के लिए स्ट्रिंग। यदि फाइल मौजूद नहीं है, तो `spit` इसे बना देगा। यदि है, तो `spit` इसे ओवरव्राइट कर देगा।

```clojure
(spit "example.txt" "नमस्ते, दुनिया!")
```

मौजूदा फाइल में टेक्स्ट जोड़ने के लिए, आप `spit` फ़ंक्शन का उपयोग `:append` विकल्प के साथ कर सकते हैं।

```clojure
(spit "example.txt" "\nआइए इस नई लाइन को जोड़ें।" :append true)
```

इन स्निपेट्स को चलाने के बाद, "example.txt" में निम्नलिखित होगा:

```
नमस्ते, दुनिया!
आइए इस नई लाइन को जोड़ें।
```

### तृतीय-पक्ष पुस्तकालयों का उपयोग
जबकि Clojure की निर्मित क्षमताएँ अक्सर पर्याप्त होती हैं, समुदाय ने अधिक जटिल या विशिष्ट कार्यों के लिए मजबूत पुस्तकालयों को विकसित किया है। फाइल I/O के लिए, एक लोकप्रिय पुस्तकालय `clojure.java.io` है, जो फाइल हैंडलिंग के लिए अधिक Java-समान दृष्टिकोण प्रदान करता है।

फाइल में लिखने के लिए `clojure.java.io` का उपयोग करने के लिए, आपको पहले इसे आयात करना होगा:

```clojure
(require '[clojure.java.io :as io])
```

फिर, आप `writer` फ़ंक्शन का उपयोग करके एक राइटर ऑब्जेक्ट प्राप्त कर सकते हैं, और फाइल में लिखने के लिए `spit` फ़ंक्शन (या अन्य जैसे `print`, `println`) का उपयोग कर सकते हैं:

```clojure
(with-open [w (io/writer "example_with_io.txt")]
  (.write w "यह clojure.java.io का उपयोग करके लिखा गया है"))
```

इससे "example_with_io.txt" में टेक्स्ट के साथ (या अगर पहले से मौजूद है, तो ओवरराइट कर देगा) बना देगा:

```
यह clojure.java.io का उपयोग करके लिखा गया है
```

याद रखें: `with-open` सुनिश्चित करता है कि लिखने के बाद फाइल उचित रूप से बंद हो जाए, जिससे संभावित संसाधन रिसाव की संभावना कम हो जाती है।
