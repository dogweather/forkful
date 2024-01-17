---
title:                "csv के साथ काम करना"
html_title:           "Gleam: csv के साथ काम करना"
simple_title:         "csv के साथ काम करना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/working-with-csv.md"
---

{{< edit_this_page >}}

## CSV क्या है? और क्यों हम इसका प्रयोग करते हैं?
CSV (Comma Separated Values) एक फाइल फॉर्मेट है जो डेटा को संग्रहीत करने और संसाधित करने के लिए उपयोग किया जाता है। प्रोग्रामर्स CSV के साथ काम करते हैं क्योंकि यह डेटा प्रोसेसिंग और समझने को आसान बनाता है।

## कैसे करें:
```Gleam
import csv

// CSV फाइल खोलें
csv.open("file.csv")

// डेटा पढ़ें
data := csv.read()

// डेटा लिखें
csv.write(data)

// CSV फाइल बंद करें
csv.close()
```

## गहराई से जानकारी:
CSV फॉर्मेट 1972 में डेटाबेस कंपनी IBM द्वारा विकसित किया गया था। यह एक खुले स्रोत फॉर्मेट है जो डेटा संसाधित करने के लिए उपयोग किया जाता है। अन्य विकल्प CSV शामिल हैं JSON और XML। CSV कोडिंग के लिए विभिन्न लाइब्रेरी उपलब्ध हैं, जहां इस फॉर्मेट के साथ काम करने के लिए निर्दिष्ट फंक्शन दिए गए हैं।

## और भी पढ़ें:
अगर आप और भी गहराई से जानकारी चाहते हैं, तो आप इन लिंक्स पर जा सकते हैं:
- https://csvspec.com/ - CSV फॉर्मेट की स्पेसिफिकेशन
- https://github.com/sepavolvic/csv - Gleam में CSV परिचालन के लिए जानकारी।