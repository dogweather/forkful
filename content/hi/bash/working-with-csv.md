---
title:                "CSV के साथ काम करना"
date:                  2024-01-19
html_title:           "Bash: CSV के साथ काम करना"
simple_title:         "CSV के साथ काम करना"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
CSV (Comma-Separated Values) एक साधारण फाइल फॉर्मेट है जो डेटा को टेबलर फॉर्म में स्टोर करता है। प्रोग्रामर्स CSV का इस्तेमाल डेटा एक्सचेंज, विश्लेषण और डेटा मैनेजमेंट के लिए करते हैं क्योंकि यह सरल और संगत होता है।

## How to (कैसे करें):
Bash में CSV से कैसे काम करें, इसकी तरकीबें:

एक सामान्य CSV फाइल पढ़ना:
```Bash
while IFS=, read -r column1 column2 column3
do
  echo "कॉलम 1: $column1, कॉलम 2: $column2, कॉलम 3: $column3"
done < data.csv
```

डेटा को सॉर्ट और कट करना:
```Bash
sort data.csv | cut -d ',' -f2
```

## Deep Dive (गहराई में जानकारी):
CSV का इस्तेमाल 1970 के दशक से हो रहा है। यह JSON, XML जैसे मॉडर्न फॉर्मेट्स का एक विकल्प है। Bash में 'awk', 'sed' तथा 'grep' जैसे उपकरण CSV डेटा को संसाधित करने में प्रयोग किए जाते हैं।

## See Also (और जानकारी के लिए):
- Bash मैनुअल: https://www.gnu.org/software/bash/manual/
- CSV पर अधिक जानकारी: https://tools.ietf.org/html/rfc4180
- Advanced Bash-Scripting Guide: https://tldp.org/LDP/abs/html/
