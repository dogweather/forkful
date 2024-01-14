---
title:    "Clojure: टेस्ट लिखना"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

हे हिंदी पाठकों!
यदि आप एक क्लोजुर प्रोग्रामर हैं, तो आप निश्चित रूप से अपने कोड के टेस्ट लिखने की जरूरत जानते हैं। लेकिन क्या आपको पता है कि यह क्यों जरूरी है? और टेस्ट कैसे लिखें और जानकारी की गहराई में जाएं?

## क्यों
टेस्ट लिखना आपके कोड को सुरक्षित बनाने में महत्वपूर्ण भूमिका निभाता है। इससे आपको अपने कोड की गुणवत्ता, प्रवाह और प्रोग्राम की भावना को टेस्ट करने में मदद मिलती है। अतः, रिक्त टेस्ट सुनिश्चित करते हैं कि आपका कोड स्थायी है और कोड में नए परिवर्तनों के दौरान न कोई गलतियां होती हैं। 

## कैसे करें
```Clojure
(deftest add-test
  (testing "returns the sum of two numbers"
    (is (= (+ 2 3) 5))))
```

यहां हमने `add-test` नाम का टेस्ट फंक्शन बनाया है जो दो संख्याओं के जोड़ को टेस्ट करता है। `testing` फंक्शन का उपयोग हमें टेस्ट के शीर्षक को और `is` फंक्शन हमें दो संख्याओं के जोड़ का परिणाम `5` से तुलना करने को बताता है। हम `leiningen` के माध्यम से अपने कोड को टेस्ट कर सकते हैं।

## गहराई में जाएं
टेस्ट लिखने का एक अधिक गहरा अध्ययन के लिए, आप Clojure संकुचन लेकिन महत्वपूर्ण टेस्ट फ्रेमवर्क `clojure.test` को देख सकते हैं। इसमें विस्तृत टेस्ट सुविधाएं होती हैं जो आपको अपने कोड में सुधार करने में मदद करती हैं। 

## और देखें
अ