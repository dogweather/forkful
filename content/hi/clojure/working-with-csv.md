---
title:                "Clojure: कंप्यूटर प्रोग्रामिंग में csv के साथ काम करना"
simple_title:         "कंप्यूटर प्रोग्रामिंग में csv के साथ काम करना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

## क्यों

CSV फाइलें डेटा संग्रह और एनालिटिक्स में बहुत महत्वपूर्ण हैं। Clojure विकसित के द्वारा CSV फाइलों को पढ़ना और उनके साथ काम करना आसान है।

## कैसे करें

```Clojure
(require '[clojure.java.io :as io])
(require '[clojure.data.csv :as csv])

(with-open [file (io/reader "example.csv")]
  (doall (csv/read-csv file)))

```

यह उदाहरण सुनिश्चित करेगा कि आप से CSV फ़ाइल खोल सकते हैं और उसका डेटा पढ़ सकते हैं।

```Clojure
(with-open [file (io/writer "output.csv")]
  (csv/write-csv file [["Name" "Age" "City"]
                        ["John" "25" "New York"]
                        ["Jane" "30" "London"]]))

```

इस उदाहरण में, हम एक डेटा फोर्मेट करते हैं और उसे नए CSV फ़ाइल में लिखते हैं।

## गहराई में जाएं

CSV फाइलों को Clojure में उपयोग करना स्वर्गीय तरीके से सुविधाजनक है। कुछ उपयोगी कार्य शामिल हैं:

- `csv/read-csv` फ़ंक्शन पढ़ने के लिए CSV फ़ाइल से लाइन एरे लौटाता है।
- `csv/write-csv` फ़ंक्शन CSV फ़ाइल में डेटा लिखता है।
- `csv/indexed-csv` फ़ंक्शन CSV फ़ाइल से प्रत्येक पंक्ति को उनके शीर्षक द्वारा ग्रुप करता है।
- `csv/csv-input-stream` और `csv/csv-output-stream` स्ट्रीम ऑब्जेक्ट बनाते हैं जो CSV फ़ाइल से डेटा पढ़ने और उसमें प्रतिक्रिया करने को समर्थ करते हैं।

## देखें भी

- [Clojure डॉक्यूमेंटेशन](https://clojure.org/index)
- [Clojure CSV दस्तावेज़ीकरण](https://clojure.github.io/data.csv/)
- [How to Read and Write CSV Files in Clojure](https://medium.com/@Deepak_Negi/how-to-read-and-write-csv-files-in-clojure-e7cdd39529a3)