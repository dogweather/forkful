---
title:                "CSV के साथ काम करना"
aliases:
- /hi/clojure/working-with-csv/
date:                  2024-02-03T19:20:09.454613-07:00
model:                 gpt-4-0125-preview
simple_title:         "CSV के साथ काम करना"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

CSV (कॉमा-सेपरेटेड वैल्यूज़) फाइलों के साथ कार्य करना टेक्स्ट डेटा को पार्स करने और उत्पन्न करने की प्रक्रिया है जो पंक्तियों और स्तम्भों के रूप में संरचित होती है, स्प्रेडशीट डेटा के समान। यह प्रक्रिया एप्लिकेशनों, डेटाबेस और डेटा ट्रांसफॉर्मेशन कार्यों के बीच डेटा विनिमय के लिए आवश्यक है, क्योंकि CSV एक हल्का, अंतर्संचालनीय प्रारूप के रूप में व्यापक रूप से अपनाया गया है।

## कैसे करें:

### CSV फाइल पढ़ना
Clojure में अपनी स्टैण्डर्ड लाइब्रेरी में CSV पार्सिंग बिल्ट-इन नहीं है, लेकिन आप इस उद्देश्य के लिए `clojure.data.csv` लाइब्रेरी का उपयोग कर सकते हैं। पहले, अपनी प्रोजेक्ट डिपेंडेंसीज़ में इस लाइब्रेरी को जोड़ें।

अपने `project.clj` में, निम्न डिपेंडेंसी जोड़ें:
```clojure
[clojure.data.csv "1.0.0"]
```
CSV फाइल को पढ़ना और प्रत्येक पंक्ति को प्रिंट करना:
```clojure
(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(with-open [reader (io/reader "path/to/yourfile.csv")]
  (doall
   (map println (csv/read-csv reader))))
```
यह CSV की प्रत्येक पंक्ति को Clojure वेक्टर के रूप में आउटपुट देगा।

### CSV फाइल में लिखना
CSV फाइल में डेटा लिखने के लिए, आप वही `clojure.data.csv` लाइब्रेरी का उपयोग कर सकते हैं:
```clojure
(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(let [data [["id" "name" "age"]
            ["1" "John Doe" "28"]
            ["2" "Jane Doe" "31"]]]
  (with-open [writer (io/writer "path/to/outputfile.csv")]
    (csv/write-csv writer data)))
```
यह `outputfile.csv` को बनाता है या ओवरराइट करता है, उसे निर्दिष्ट डेटा से भरता है।

### तीसरे पक्ष की लाइब्रेरी का उपयोग करना: `clojure.data.csv`

जबकि `clojure.data.csv` Clojure में CSV हैंडलिंग के लिए स्पष्ट रूप में सबसे सीधी लाइब्रेरी है, अधिक जटिल कार्यों, जैसे कि विशेष वर्णों या अपरंपरागत डेलीमिटर्स के साथ CSV को हैंडल करने के लिए, आप पारिस्थितिकी तंत्र के भीतर अतिरिक्त विकल्पों का पता लगा सकते हैं या यहाँ तक कि Apache Commons CSV जैसी लाइब्रेरीज़ के साथ Java इंटरॉ�प को विचारित कर सकते हैं। हालांकि, Clojure में अधिकांश मानक CSV प्रोसेसिंग कार्यों के लिए, `clojure.data.csv` एक सरल और प्रभावी टूलसेट प्रदान करता है।
