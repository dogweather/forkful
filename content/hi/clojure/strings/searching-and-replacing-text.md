---
title:                "पाठ खोजना और बदलना"
aliases:
- /hi/clojure/searching-and-replacing-text/
date:                  2024-01-20T17:57:33.688320-07:00
model:                 gpt-4-1106-preview
simple_title:         "पाठ खोजना और बदलना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
टेक्स्ट खोजना और बदलना यानी किसी स्ट्रिंग के भीतर विशिष्ट शब्द या पैटर्न को ढूँढकर उन्हें दूसरे शब्दों से बदल देना। प्रोग्रामर्स डाटा संशोधन, कोड रिफैक्टरिंग, और ऑटोमेशन में इसका उपयोग करते हैं।

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
