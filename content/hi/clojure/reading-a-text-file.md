---
title:                "Clojure: एक टेक्स्ट फ़ाइल पढ़ना"
simple_title:         "एक टेक्स्ट फ़ाइल पढ़ना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Whyक्यों: एक पाठ फ़ाइल को पढ़ने के लिए कोई दिलचस्पी रखने के लिए *क्यों* कोडिंगवादकों के लिए उपयुक्त है। इसके अलावा, यह जानना भी आवश्यक है कि पाठ फ़ाइलों में किसी भी तरह की डेटा को कैसे और जानकारी को कैसे पढ़ा जाए।

## How To

"```Clojure
(defn read-text-file [file]
  (with-open [reader (clojure.java.io/reader file)]
    (doseq [line (line-seq reader)]
      (println line)))

(read-text-file "sample.txt")

;;output:
;;This is a sample text file.
;;It contains multiple lines.
;;Each line is read and printed using Clojure.
```"

## Deep Dive

पाठ फ़ाइलों को पढ़ने के लिए, क्लोजुर में विभिन्न फ़ंक्शन्स का प्रयोग किया जाता है। सबसे पहले, हम ```with-open``` फ़ंक्शन का उपयोग करके एक फ़ाइल रीडर खोलते हैं और उसे एक एलियास में स्थापित करते हैं। फिर, हम ```line-seq``` फ़ंक्शन का उपयोग करके रीडर में से पंक्तियों की एक श्रृंखला बनाते हैं। और अंत में, हम ```doseq``` फ़ंक्शन का उपयोग करके उस श्रृंखला के हर पंक्ति को प्रिंट करते हैं। इस तरह, हम पाठ फ़ाइल को सुचारू ढंग से पढ़ सकते हैं।

## See Also

- [Official Clojure Documentation on File I/O](https://clojure.org/reference/java_interop#_file_i_o) 
- [Clojure Cookbook Chapter on File Reading and Writing](https://clojure-cookbook.net/file-io/file_reading_writing.html)