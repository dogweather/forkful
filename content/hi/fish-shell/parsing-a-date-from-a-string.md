---
title:                "स्ट्रिंग से दिनांक पार्स करना"
date:                  2024-01-20T15:36:31.973220-07:00
html_title:           "Arduino: स्ट्रिंग से दिनांक पार्स करना"
simple_title:         "स्ट्रिंग से दिनांक पार्स करना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
एक तारीख को स्ट्रिंग से पार्स करने का मतलब है उसे ऐसे फॉर्मेट में बदलना जिसे कंप्यूटर समझ सके और इस्तेमाल कर सके। प्रोग्रामर्स इसे इसलिए करते हैं ताकि वे तारीखों की तुलना, गणना और मैनिपुलेशन कर सकें।

## कैसे करें:
Fish Shell में तारीख पार्स करना:

```Fish Shell
set date_string "2023-03-15"
set epoch_time (date -ud "$date_string" +"%s")
echo $epoch_time
```

आउटपुट:

```
1678847400
```

## गहराई से जानकारी:
समय के साथ, शैल स्क्रिप्टिंग में तारीख पार्सिंग सुधार होते रहे हैं। UNIX में `date` कमांड का इस्तेमाल होता आया है, जो GNU `date` में और विशेषताओं के साथ आता है। विकल्पों में `strptime` और भाषा-विशिष्ट फंक्शंस जैसे कि Python में `datetime.strptime` हैं। Fish में, हम आमतौर पर `date` कमांड का उपयोग करते हैं, जो कि डेटा और समय को मानव-पठनीय फॉर्मेट से Unix Epoch टाइम में बदलने के लिए उपयोगी है, जिसे सिस्टम और स्क्रिप्ट्स आसानी से समझ और प्रोसेस कर सकती हैं।

## यह भी देखें:
- GNU `date` मैन्युअल: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- Fish Shell Documentation: https://fishshell.com/docs/current/index.html
- Epoch Converter: https://www.epochconverter.com/
