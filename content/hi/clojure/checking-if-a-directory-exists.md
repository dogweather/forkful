---
title:                "डायरेक्टरी मौजूद है या नहीं की जांच"
html_title:           "Clojure: डायरेक्टरी मौजूद है या नहीं की जांच"
simple_title:         "डायरेक्टरी मौजूद है या नहीं की जांच"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

रणनीति जांचें कि क्या निर्देशिका मौजूद है और क्यों करते हैं इसका कारण है कि इस लेख में हम आपको बताएंगे।

## क्या और क्यों?

निर्देशिका मौजूद है जैसे जांचना एक रणनीति है जो रेखाएं सुनिश्चित करता है कि एक निर्देशिका क्या संख्या मौजूद है या नहीं। यह प्रोग्रामर्स अपने कोड में शामिल करते हैं क्योंकि यह अपनी सुनिश्चित करता है कि उनका कोड उचित ढंग से काम कर रहा है और निर्देशिका अगर मौजूद नहीं है तो वे उचित तरीके से हांगोउप हो सकते हैं।

## पहले यह करें:

आपके कोड में निर्देशिका का अस्तित्व जांचने के लिए निम्नलिखित आसान से है:
```Clojure
(defn dir-exists? [dir-path]
  (clojure.java.io/file dir-path))
```

तो आप उसे उपयोग कर सकते हैं और वह आपको सुनिश्चित करता है कि निर्देशिका मौजूद है या नहीं।

## गहराई खोज

यह रणनीति पहले से ही बहुत समय से प्रोग्रामिंग में उपयोग किया जाता है और आम रूप से सबसे अच्छा तरीका है एक दिए हुए डिरेक्टिव के अस्तित्व को जाँचने का। अन्य विकल्पों में, आप एक निर्देशिका में एक खाली निर्देशिका की जांच कर सकते हैं, लेकिन ऐसा करने में आपको कुछ एक्सट्रा कोड को लिखने की जरूरत होती है। इसलिए, रणनीति का उपयोग करना अपनी सरलता के कारण एक बेहतर विकल्प है।

## उसे भी देखें:

- [Official Clojure Documentation](https://clojure.org/reference/java_interop#file)
- [Simple ways to check if a folder exists in Clojure](https://www.baeldung.com/clojure/check-folder-exists)
- [Check if a directory exists in Java](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)