---
title:                "एक निर्देशिका मौजूद है या नहीं, इसकी जाँच करना"
html_title:           "Clojure: एक निर्देशिका मौजूद है या नहीं, इसकी जाँच करना"
simple_title:         "एक निर्देशिका मौजूद है या नहीं, इसकी जाँच करना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## क्या एवं क्यों? (What & Why?)

डायरेक्टरी मौजूद है होने की जांच आपको यह बताती है कि किसी विशेष फ़ाइल सिस्टम पथ पर कोई डायरेक्टरी मौजूद है या नहीं। यह तब आवश्यक होता है जब किसी फ़ाइल को पढ़ना, लिखना या बनाने का प्रयास किया जाता है, ताकि गलतियां या अपवादों को पहले ही रोका जा सके।

## कैसे: (How to:)

Clojure में, हम इसे `clojure.java.io/file` और `exists?` का उपयोग करके चेक कर सकते हैं।

```Clojure
(require '[clojure.java.io :as io])

(defn directory-exists? [dir]
  (and (.exists (io/file dir)) (.isDirectory (io/file dir))))

(directory-exists? "path/to/directory")
```

यदि डायरेक्टरी 'path/to/directory' मौजूद है, तो यह `true` लौटाएगा, अन्यथा `false`.

## गहराई में: (Deep Dive)

**ऐतिहासिक संदर्भ:** Clojure 2007 में जारी हुआ था और इसने जावा की बहुत सारी क्षमताओं को अदान-प्रदान किया। इसलिए, हम जावा कोड को एक्सेस और इस्तेमाल कर सकते हैं, जैसे कि डायरेक्टरी मौजूद होने की जांच करना।

**विकल्प:** डायरेक्टरी की मौजूदगी की जाँच करने के कई तरीके हैं। `java.nio.file.Files` का उपयोग करना एक अन्य विकल्प हो सकता है। 

**कार्यान्वयन विवरण:** `io/file dir` हमें एक जावा `File` ऑब्जेक्ट देगा। `.exists` और `.isDirectory` दो जावा मेथड हैं जो `true` या `false` लौटाते हैं। जब दोनों `true` होते हैं, हमें पता चलता है कि ये एक मौजूदा डायरेक्टरी है।

## अधिक देखें: (See Also)

- जावा मेथड का उपयोग करते हुए डायरेक्टरी की जाँच करने के बारे में अधिक जानकारी के लिए: [Java Documentation](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)
- Clojure की अन्य फ़ाइल ओपरेशन्स: [Clojure I/O](https://clojure.org/reference/io)