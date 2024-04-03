---
date: 2024-01-20 17:57:33.688320-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) ."
lastmod: '2024-03-13T22:44:51.637577-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u092A\u093E\u0920 \u0916\u094B\u091C\u0928\u093E \u0914\u0930 \u092C\u0926\
  \u0932\u0928\u093E"
weight: 10
---

## How to: (कैसे करें:)
```clojure
;; एक सिंपल टेक्स्ट रिप्लेसमेंट
(defn replace-text [text old-part new-part]
  (clojure.string/replace text old-part new-part))

;; उदाहरण
(replace-text "Clojure मजेदार है!" "मजेदार" "शक्तिशाली")
;; Output: "Clojure शक्तिशाली है!"

;; रेगेक्स का उपयोग करते हुए रिप्लेसमेंट
(defn regex-replace-text [text pattern replacement]
  (clojure.string/replace text (re-pattern pattern) replacement))

;; उदाहरण
(regex-replace-text "123-456-7890" "\\d" "*")
;; Output: "***-***-****"
```

## Deep Dive (गहराई में जानकारी):
टेक्स्ट खोजने और बदलने का कार्य प्रोग्रामिंग में बहुत पुराना है, शुरुआती दिनों से ही एडिटर्स और डेवलपमेंट टूल्स में इसे एक मूलभूत सुविधा के रूप में देखा गया। इस काम को shell commands जैसे `sed` और टेक्स्ट एडिटर्स जैसे `vim` या `emacs` भी कर सकते हैं। Clojure में, `clojure.string/replace` और `re-pattern` फंक्शंस इसे आसान बनाते हैं। जब रेगेक्स का उपयोग होता है, तो आपके पास अधिक शक्तिशाली पैटर्न मैचिंग की क्षमता होती है जिससे जटिल जानकारी को भी बदलना संभव हो जाता है।

## See Also (और भी देखें):
- Clojure.string API डॉक्युमेंटेशन: [Clojure Docs](https://clojuredocs.org/clojure.string/replace)
- RegexOne [RegexOne](https://regexone.com/) पर रेगेक्स सीखें।
- Clojure के लिए इंटरएक्टिव एक्सरसाइज: [4Clojure](https://www.4clojure.com/)
