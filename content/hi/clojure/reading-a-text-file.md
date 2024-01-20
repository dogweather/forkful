---
title:                "एक पाठ फ़ाइल पढ़ना"
html_title:           "Bash: एक पाठ फ़ाइल पढ़ना"
simple_title:         "एक पाठ फ़ाइल पढ़ना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

टेक्स्ट फ़ाइल पढ़ना मतलब होता है किसी फ़ाइल से डाटा खींचना। प्रोग्रामर्स इसे क्यों करते हैं? त्वरित डेटा प्रक्रिया, निर्दिष्ट फ़ाइल में संग्रहीत डेटा संग्रहित करने के लिए।

## कैसे:

Clojure में टेक्स्ट फ़ाइल पढ़ने का तरीका:

```Clojure
(with-open [reader (clojure.java.io/reader "फ़ाइल.txt")]
  (doseq [line (line-seq reader)]
    (println line)))
```

इस कोड का परिणाम होगा:
```Clojure
लाइन1
लाइन2
```

## गहरा डाइव

अगर हम यह कहें की Clojure में टेक्स्ट फ़ाइल पढ़ना एक ऐतिहासिक परिप्रेक्ष्य में बहुत महत्वपूर्ण रहा है, तो यह गलत नहीं होगा। क्योंकि, डाटा जो आपके पास होता है, वह अक्सर दस्तावेज़ों, डेटाबेस, या JSON फ़ाइलों में संग्रहित हो सकता है, लेकिन इनमें से सभी चीज़ों को आपको Clojure में परिवर्तित करना होता है। 

हालांकि, एक और विकल्प भी है, जिसे `slurp` कहा जाता है, जो फ़ाइल की सभी लाइन्स को एक समझने लायक तरीके से एकत्रित करता है जैसे - 

```Clojure
(println (slurp "फ़ाइल.txt"))
```

लेकिन, `slurp` का उपयोग तब करना उचित होता है, जब फ़ाइल छोटी हो, क्योंकि यह पूरी फ़ाइल को मेमोरी में लोड करता है।

## देखने के लिए भी

[Clojure Docs](https://clojure.org/api/cheatsheet) और [Clojure Cookbook](https://books.google.co.in/books?id=_FqtBAAAQBAJ&printsec=frontcover#v=onepage&q&f=false) में और एडवांस्ड विषयों पर और अतिरिक्त जानकारी है। इन्हें जरूर देखें।