---
title:                "स्ट्रिंग से दिनांक पार्स करना"
date:                  2024-01-20T15:34:43.464844-07:00
html_title:           "Arduino: स्ट्रिंग से दिनांक पार्स करना"
simple_title:         "स्ट्रिंग से दिनांक पार्स करना"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
तारीख को स्ट्रिंग से पार्स करना यानी उसे एक पाठ्य रूप से निकालकर किसी मान्य स्वरूप में बदलना होता है। प्रोग्रामर इसे इसलिए करते हैं ताकि विभिन्न स्रोतों से प्राप्त तारीख की जानकारी को सामान्यीकृत और संभालने में आसान बनाया जा सके।

## कैसे करें? (How to:)
Bash में तारीख पार्स करने का सीधा उदाहरण है `date` कमांड का उपयोग करना:

```Bash
# स्ट्रिंग से ISO फॉर्मेट में तारीख पार्स करें
parsed_date=$(date -d '2023-03-15 14:23:00' '+%Y-%m-%d %H:%M:%S')
echo $parsed_date
```
आउटपुट:
```
2023-03-15 14:23:00
```

आप `date` कमांड के विभिन्न विकल्पों का उपयोग कर तारीख के स्वरूप को बदल भी सकते हैं।

## गहराई से समझें (Deep Dive)
Bash में `date` कमांड 1970s से उपयोग हो रही है। यह सिस्टम की घड़ी से समय प्राप्त करने और मानकीकरण करने के लिए है। अलग-अलग लोकेल और टाइमज़ोन के समर्थन के लिए, GNU `date` में `-d` विकल्प का प्रयोग होता है ताकि अर्बिट्रेरी तिथि-समय स्ट्रिंग की पार्सिंग कर सकें।

विकल्प जैसे `dateutils`, `GNU coreutils` या पाइथन की `datetime` मॉड्यूल भी पार्सिंग के लिए प्रयोग किए जा सकते हैं जब Bash नापर्याप्त हो।

लक्ष्य सिर्फ तारीख को स्वरूपित करना नहीं, बल्कि उसे किसी ऐसे प्रारूप में बदलना होता है, जिसे सिस्टम, डाटाबेस या अन्य एप्लिकेशन में आसानी से पढ़ा और संग्रहीत किया जा सके।

## संबंधित सूत्र (See Also)
- बैश मेनुअल: https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html
- `date` कमांड ट्यूटोरियल: https://www.cyberciti.biz/tips/linux-unix-get-date-formatting-tips-for-sys-admins.html
- GNU coreutils: https://www.gnu.org/software/coreutils/manual/html_node/index.html