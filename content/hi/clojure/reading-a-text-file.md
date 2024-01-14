---
title:    "Clojure: एक पाठ फ़ाइल पढ़ना"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## क्यों

एक टेक्स्ट फ़ाइल पढ़ने में जुटने का कारण समझने के लिए ही दूसरों के कड़वे शब्दों सुनने से बेहतर है।

## कैसे करें

```Clojure
;; एक टेक्स्ट फ़ाइल को खोलें
(with-open [file (clojure.java.io/reader "myfile.txt")]
  (doseq [line (line-seq file)]
    (println line)))
```

यह कोड उपरोक्त फ़ाइल से प्रत्येक पंक्ति को प्रिंट करेगा।

## गहराई में डूबना

आप अपनी क्षमताओं को अधिक उपयोगी बनाने के लिए टेक्स्ट फ़ाइलों को पढ़ना सीख सकते हैं। आप उन्हें पार्स कर, डाटाबेस में सहेज सकते हैं और अन्य निर्देशों के साथ काम कर सकते हैं। 

## देखें भी

[Java IO दस्तावेज़](https://docs.oracle.com/javase/7/docs/api/java/io/package-summary.html) 

[Clojure दस्तावेज़](https://clojure.org/reference/io)