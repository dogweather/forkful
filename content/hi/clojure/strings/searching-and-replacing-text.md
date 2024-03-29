---
date: 2024-01-20 17:57:33.688320-07:00
description: "\u091F\u0947\u0915\u094D\u0938\u094D\u091F \u0916\u094B\u091C\u0928\u093E\
  \ \u0914\u0930 \u092C\u0926\u0932\u0928\u093E \u092F\u093E\u0928\u0940 \u0915\u093F\
  \u0938\u0940 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u0947 \u092D\
  \u0940\u0924\u0930 \u0935\u093F\u0936\u093F\u0937\u094D\u091F \u0936\u092C\u094D\
  \u0926 \u092F\u093E \u092A\u0948\u091F\u0930\u094D\u0928 \u0915\u094B \u0922\u0942\
  \u0901\u0922\u0915\u0930 \u0909\u0928\u094D\u0939\u0947\u0902 \u0926\u0942\u0938\
  \u0930\u0947 \u0936\u092C\u094D\u0926\u094B\u0902 \u0938\u0947 \u092C\u0926\u0932\
  \ \u0926\u0947\u0928\u093E\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\
  \u092E\u0930\u094D\u0938 \u0921\u093E\u091F\u093E \u0938\u0902\u0936\u094B\u0927\
  \u0928, \u0915\u094B\u0921\u2026"
lastmod: '2024-03-13T22:44:51.637577-06:00'
model: gpt-4-1106-preview
summary: "\u091F\u0947\u0915\u094D\u0938\u094D\u091F \u0916\u094B\u091C\u0928\u093E\
  \ \u0914\u0930 \u092C\u0926\u0932\u0928\u093E \u092F\u093E\u0928\u0940 \u0915\u093F\
  \u0938\u0940 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u0947 \u092D\
  \u0940\u0924\u0930 \u0935\u093F\u0936\u093F\u0937\u094D\u091F \u0936\u092C\u094D\
  \u0926 \u092F\u093E \u092A\u0948\u091F\u0930\u094D\u0928 \u0915\u094B \u0922\u0942\
  \u0901\u0922\u0915\u0930 \u0909\u0928\u094D\u0939\u0947\u0902 \u0926\u0942\u0938\
  \u0930\u0947 \u0936\u092C\u094D\u0926\u094B\u0902 \u0938\u0947 \u092C\u0926\u0932\
  \ \u0926\u0947\u0928\u093E\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\
  \u092E\u0930\u094D\u0938 \u0921\u093E\u091F\u093E \u0938\u0902\u0936\u094B\u0927\
  \u0928, \u0915\u094B\u0921\u2026"
title: "\u092A\u093E\u0920 \u0916\u094B\u091C\u0928\u093E \u0914\u0930 \u092C\u0926\
  \u0932\u0928\u093E"
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
