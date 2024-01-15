---
title:                "एक अस्थायी फाइल बनाना"
html_title:           "Clojure: एक अस्थायी फाइल बनाना"
simple_title:         "एक अस्थायी फाइल बनाना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## क्यों

आपने पहले कभी temporary file के बारे में सुना होगा। यह एक अस्थायी फाइल होती है जो काम करने के बाद खुद बन्द हो जाती है। यह फाइलें फाइल सिस्टम पर स्थान का इस्तेमाल कम करती हैं और अनुरोध पर बनाई जाती हैं, जो अनिश्चित समय के लिए हो सकता है। इसलिए, इसका इस्तेमाल नए डेटा को सिस्टम पर स्थान का इस्तेमाल किए बिना आसानी से शुरू करने के लिए किया जाता है।

## कैसे

एक temporary file बनाने के लिए, हम απροχωρητος नामक फंक्शन का इस्तेमाल कर सकते हैं। इसका उपयोग निम्नलिखित स्टेटमेंट के साथ किया जा सकता है:

```Clojure
(with-open [temp-file (java.io.File/createTempFile "prefix" "suffix")]
  (do-something-with temp-file)
  (.delete temp-file))
```

आप ऊपर दिए गए कोड ब्लॉक में स्टेटमेंट को अपनी आवश्यकतानुसार संशोधित कर सकते हैं। इसमें `prefix` और `suffix` आपके temporary file के नाम को निर्धारित करने के लिए हैं। आप फाइल को उपयोग करने के बाद, उसे .delete फंक्शन से हटा सकते हैं। यदि आप .delete को कॉल नहीं करते हैं तो temporary file स्वचालित रूप से हट गई जाएगी।

## गहराई तक जाएं

आप temporary file में डेटा लिख सकते हैं और पढ़ सकते हैं उसके लिए आप मूल फंक्शन `java.io.RandomAccessFile` का इस्तेमाल कर सकते हैं। आप निम्नलिखित कोड में दिखाए गए `do-something-with` फंक्शन को इस्तेमाल कर सकते हैं:

```Clojure
(with-open [temp-file (java.io.File/createTempFile "prefix" "suffix")]
  (let [random-file (java.io.RandomAccessFile. temp-file "rw")]
    (.writeInt random-file 25)
    (.writeBoolean random-file true)
    (.writeUTF random-file "नमस्ते")
    (.