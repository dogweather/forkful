---
title:                "CSV के साथ काम करना"
html_title:           "Bash: CSV के साथ काम करना"
simple_title:         "CSV के साथ काम करना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

CSV, यानी Comma-Separated Values, एक साधारण फॉर्मेट है जो टेबल डाटा को टेक्स्ट फाइल में स्टोर करती है। प्रोग्रामर्स इसका इस्तेमाल डाटा को आसानी से एक्सचेंज, स्टोर और प्रोसेस करने के लिए करते हैं। 

## How to: (कैसे करें:)

Fish Shell में CSV फाइल को पढ़ने, लिखने और प्रोसेस करने के उदाहरण:

```Fish Shell
# CSV फ़ाइल के हर लाइन को पढ़ें
cat data.csv | while read line
    echo $line
end

# CSV से पहला कॉलम निकालना
cut -d ',' -f1 data.csv

# CSV को sort करना दूसरे कॉलम के आधार पर
sort -t ',' -k2,2 data.csv
```

सैंपल आउटपुट:

```Fish Shell
नाम,उम्र,शहर
रोहन,25,दिल्ली
आयुषी,22,मुंबई
```

## Deep Dive (गहराई में जानकारी):

CSV फॉर्मेट 1972 में IBM Fortran (level G) कंपाइलर में पेश की गई थी। एक्सेल और Google Sheets जैसे टूल्स ने CSV को और बढ़ावा दिया। इसके विकल्प के रूप में JSON, XML, और YAML हैं, जो अधिक स्ट्रक्चर्ड डाटा को हैंडल करते हैं। Fish Shell में CSV की प्रोसेसिंग `awk`, `grep`, `cut` जैसे कमांड-लाइन टूल्स का उपयोग कर सिंपल हो जाती है।

## See Also (और भी जानें):

- CSV प्रोसेसिंग के लिए अधिक Fish Shell उदाहरण: [https://fishshell.com/docs/current/](https://fishshell.com/docs/current/)
- CSV फाइल फॉर्मेट की जानकारी: [https://tools.ietf.org/html/rfc4180](https://tools.ietf.org/html/rfc4180)
- awk, grep, और cut कमांड्स के ट्यूटोरियल्स: [https://www.gnu.org/software/grep/manual/grep.html](https://www.gnu.org/software/grep/manual/grep.html)
