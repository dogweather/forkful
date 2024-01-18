---
title:                "स्ट्रिंग से तिथि तड़ना"
html_title:           "Haskell: स्ट्रिंग से तिथि तड़ना"
simple_title:         "स्ट्रिंग से तिथि तड़ना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
एक दिनांक को एक स्ट्रिंग से पार्स करना होता है, यह प्रोग्रामरों का एक आम कार्य है क्योंकि अपनी कोड को चलाने के लिए वे दिनांक डेटा को स्ट्रिंग से प्राप्त करते हैं।

## कैसे करें:
पार्सिंग एक दिनांक को स्ट्रिंग से, हास्केल में बहुत ही आसान है। नीचे दिए गए कुछ उदाहरण देखें। 
```Haskell
-- दिनांक को तारीख़ और समय के रूप में पार्स करें 
parseTimeM True defaultTimeLocale "%F %H:%M:%S" "2021-07-18 09:00:00" :: Maybe UTCTime ⇒ Just 2021-07-18 09:00:00 UTC

-- तारीख़ और समय दोनों को अलग-अलग पार्स करें 
parseTimeM True defaultTimeLocale "%F" "2021-07-18" :: Maybe Day ⇒ Just 2021-07-18
parseTimeM True defaultTimeLocale "%H:%M:%S" "09:00:00" :: Maybe TimeOfDay ⇒ Just 09:00:00
```

## गहराई में जाईए:
दिनांक को स्ट्रिंग से पार्स करने का इतिहास पुराना है। इसके विकल्प जैसे कि रेगुलर एक्सप्रेशन और पार्सर और इसके विभिन्न लाइब्रेरी से आसान है। स्ट्रिंग को पार्स करने के लिए हास्केल में एक अधिकृत प्यार्स मॉनाड उपलब्ध है। इससे हम परिणाम को आसानी से स्वीकार कर सकते हैं।

## इससे जुड़े लिंक:
सम्पूर्ण एलआई समझने के लिए, [हास्केल डेटा समय लाइब्रेरी](https://hackage.haskell.org/package/time) को जांचें। दिनांक को स्ट्रिंग से पार्स करने के लिए अधिक उदाहरण और कोड स्रोत के लिए [हास्केल विकि](https://wiki.haskell.org/Parsing_date_strings_in_reverse) को देखें।