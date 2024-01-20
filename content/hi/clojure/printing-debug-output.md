---
title:                "डीबग आउटपुट प्रिंट करना"
html_title:           "Gleam: डीबग आउटपुट प्रिंट करना"
simple_title:         "डीबग आउटपुट प्रिंट करना"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

डिबग आउटपुट प्रिंट करने का तात्पर्य सीधे तौर पर एक प्रोग्राम के आंतरिक कार्यान्वयन की जांच करने से होता है। yes, it is programmed as much. यही कारण है कि प्रोग्रामर्स इसे करते हैं: यह उन्हें त्रुटियां सुधारने में सहायता करता है और उन्हें मार्गदर्शन करता है कि कौन से भाग काम कर रहे हैं और कौन से नहीं।

## कैसे करें:

Clojure में, आप `println` फ़ंक्शन का उपयोग करके डिबग संदेशों को प्रिंट कर सकते हैं।
```Clojure
(defn sample-function [x]
  (println "Debug: Entering sample-function")
  (* x x))
```
और जब आप `sample-function` चलाते हैं, आपको निम्नलिखित आउटपुट मिलेगा:
```Clojure
(Debug: Entering sample-function)
```
## गहरी जानकारी

डिबग आउटपुट का इतिहास संगणक के इतिहास के साथ ही शुरू होता है। किसी भी भाषा में, डेवलपर्स को अपने कोड को ट्रैक करने और उससे डिबग करने की आवश्यकता होती है। Clojure में, आपके पास `println` के अलावा भी कुछ विकल्प हो सकते हैं, जैसे कि `print`, `pr`, और `printf`. `print` और `printf` का कार्य `println` के समान होता है, लेकिन `println` के विपरीत, वे नयी लाइन नहीं डालते हैं. 

## अन्य जानकारी 

प्रिंटलें और डिबगगिंग के बारे में अधिक जानकारी के लिए, आप निम्नलिखित लिंकों पर जा सकते हैं:

- Clojure का अधिकारिक डॉक्यूमेंटेशन: [https://clojure.org](https://clojure.org)
- Clojure प्रिंटलें और डिबग: [https://clojuredocs.org/clojure.core/println](https://clojuredocs.org/clojure.core/println)