---
title:                "एक तारीख को स्ट्रिंग में परिवर्तित करना"
html_title:           "Java: एक तारीख को स्ट्रिंग में परिवर्तित करना"
simple_title:         "एक तारीख को स्ट्रिंग में परिवर्तित करना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

'Date to string conversion' से आप टाइमस्टैंप को पड़ सकते हैं और ऐसा करने से किसी विशेष दिनांक पर तारीख को व्यापकता मिलती है। प्रोग्रामर्स इसे डाटा माइनिंग, लॉगिंग और डेटाबेस संचित करने के लिए करते हैं।

## कैसे करें:

Clojure में आप `clj-time` लाइब्ररी का उपयोग करके दिनांक को स्ट्रिंग में परिवर्तित कर सकते हैं:

```clojure
(defn convert-date-to-string [date]
  (-> date
      (coerce/to-string) ))
```

यहाँ, `date` पैरामीटर, जिसकी आपको स्ट्रिंग की आवश्यकता होती है, यह होता है।

उदाहरण:

```clojure
(def date-example (org.joda.time.DateTime/now))
(println (convert-date-to-string date-example))
```

यह स्ट्रिंग उत्पादन देगा जो करंट डेट और समय को दर्शाता है।

## गहराई में:

Date-to-string कनवर्जन का उपयोग पहले से ही कई भाषाओं में किया जा रहा था। Clojure ने clj-time लाइब्ररी को जोड़ा, जो जेवा 8 की डेट-टाइम API का विस्तार करती है। वैकल्पिक रूप से, आप Java interop का उपयोग करके java.text.SimpleDateFormat का उपयोग कर सकते हैं। क्लोज़र का डेट-टाइम कनवर्जन यूनिक्स टाइमस्टैंप्स पर आधारित है, जो एक सरल काउंटर है जो एक निश्चित समय को दर्शाता है।

## देखने के लिए:

* [Clojure by Example](http://kimh.github.io/clojure-by-example/#date-time)
* [Clojure: clj-time Github](https://github.com/clj-time/clj-time)
* [Clojure Official Docs](https://clojure.org/index)