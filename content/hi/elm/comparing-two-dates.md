---
title:                "Elm: दो तारीखों की तुलना करना"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## क्यों

क्या आप कभी दो तारीखों को तुलना करने के लिए कोशिश की है? शायद आप एक प्रोग्राम में दो तारीखों को बंद करने के लिए कोशिश कर रहे हैं या आप सिस्टम में तारीख से जुड़ी शुरुआती और समाप्ति तिथियों को बनाना चाहते हैं। इस लेख में, हम तारीखों को तुलना करने के लिए Elm प्रोग्रामिंग में क्या कर सकते हैं, उसके बारे में बात करेंगे।

## कैसे करें

कोड के माध्यम से तारीखों को तुलना करना Elm में बहुत आसान है। हम `Date.comparedTo` फंक्शन का उपयोग करके दो तारीखों को तुलना कर सकते हैं और उनका तुलनाना (comparing) कर सकते हैं। नीचे एक मात्र उदाहरण दिया गया है।

```Elm
firstDate = Date.fromCalendarDate 2020 4 3
secondDate = Date.fromCalendarDate 2020 4 5
compared = Date.comparedTo firstDate secondDate --returns LT (less than)
```

यहां, `compared` मान (value) `LT` (less than) को प्रस्तुत करेगा क्योंकि `firstDate` `secondDate` से कम है। इस तरह से, हमें दो तारीखों को कैसे तुलना करना है, उसके बारे में स्पष्टता प्राप्त हो जाती है।

## गहराई में जाएँ

कोडिंग के लिए आपको पहले से ही दो तारीखों को तुलना करने के उपरांत उसे तुलनाना (comparing) करने के साथ ही कुछ और चीजें भी कर सकते हैं। हम `Date.Comparable` मॉड्यूल में दो तारीखों के बीच तुलनाना करने के कई तरीके भी देखेंगे, जिनमें समय जोड़ना, गणना (calculated) और अन्य उपयोगी चरण (operations) शामिल हैं।

अधिक जानकारी के लिए