---
title:                "CSV के साथ काम करना"
date:                  2024-01-19
simple_title:         "CSV के साथ काम करना"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
CSV यानि "Comma-separated values" फाइलें होती हैं, जिनमें डेटा अल्पविराम से अलग होता है। कार्यक्रमकार इनका इस्तेमाल डेटा को आसानी से निर्यात और आयात करने के लिए करते हैं क्योंकि ये सादारण स्वरूप में होते हैं और विभिन्न भाषाओं एवं अनुप्रयोगों में इस्तेमाल में आसान होते हैं।

## How to: (कैसे करें:)
Clojure में CSV डेटा के साथ काम करने के लिए, `clojure.data.csv` लाइब्रेरी का इस्तेमाल करते हैं:

```Clojure
(require '[clojure.data.csv :as csv])
(require '[clojure.java.io :as io])

; CSV फाइल पढ़ने के लिए:
(with-open [reader (io/reader "data.csv")]
  (doall (csv/read-csv reader)))

; CSV फाइल लिखने के लिए:
(with-open [writer (io/writer "output.csv")]
  (csv/write-csv writer [["name", "age", "city"]
                         ["Amit", "30", "Delhi"]
                         ["Sara", "45", "Mumbai"]]))
```

उदाहरण का उत्पादन:
data.csv में पढ़ी गई लाइन्स की एक सूची होगी, और output.csv में लिखा डेटा इस प्रकार होगा -

```
name,age,city
Amit,30,Delhi
Sara,45,Mumbai
```

## Deep Dive (गहन जानकारी)
CSV फॉर्मेट 1970 के दशक से इस्तेमाल में है। यह बहुत फ्लेक्सिबल है, पर कभी-कभी यह दुविधा का कारण भी बन सकता है, जैसे कि डेटा में अल्पविराम की मौजूदगी। Clojure जैसे फंक्शनल भाषाएँ CSV डेटा की हैंडलिंग में उत्तम होती हैं क्योंकि ये इम्म्यूटेबिलिटी और सिक्वेंशियल प्रोसेसिंग पर जोर देते हैं। विकल्प में JSON, XML जैसी डेटा फार्मेट हैं जो अधिक संरचनात्मक जानकारी प्रदान करते हैं। CSV की सरलता उसे डेटा विश्लेषण और मशीन लर्निंग में लोकप्रिय बनाती है।

## See Also (अन्य स्रोत)
- Clojure की अधिकृत CSV लाइब्रेरी डाक्युमेन्टेशन: [https://github.com/clojure/data.csv](https://github.com/clojure/data.csv)
- Clojure डेटा प्रोसेसिंग ट्यूटोरियल: [https://www.braveclojure.com](https://www.braveclojure.com)
- CSV पर W3C मानक: [https://www.w3.org/TR/tabular-data-primer/](https://www.w3.org/TR/tabular-data-primer/)
