---
title:                "JSON के साथ काम करना"
date:                  2024-01-19
html_title:           "Arduino: JSON के साथ काम करना"
simple_title:         "JSON के साथ काम करना"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
JSON, जिसे JavaScript Object Notation कहते हैं, डाटा अदान-प्रदान का एक आम फॉर्मेट है। प्रोग्रामर्स इसे इसलिए यूज़ करते हैं क्योंकि यह हल्का, पढ़ने में आसान और भाषाओं के बीच डेटा साझा करने में लचीला होता है।

## How to: (कैसे करें:)
Fish Shell में JSON से निपटने के लिए `jq` का उपयोग कर सकते हैं।

बेसिक JSON पार्सिंग का उदाहरण:
```Fish Shell
echo '{"name": "अजय", "age": 35}' | jq '.name'
```
यह आउटपुट देगा:
```
"अजय"
```

एक JSON फाइल से डेटा एक्सट्रैक्ट करना:
```Fish Shell
cat उपयोगकर्ता.json | jq '.[0].email'
```
यह फाइल के पहले ऑब्जेक्ट का ईमेल आउटपुट करेगा। 

JSON एरे में इलिमेंट्स जोड़ना:
```Fish Shell
echo '[1, 2]' | jq '. + [3]'
```
आउटपुट होगा:
```
[1, 2, 3]
```

## Deep Dive (गहराई से जानकारी)
JSON को डगलस क्रॉकफोर्ड ने 2001 में परिचित किया था। यह XML का एक हल्का विकल्प बन गया क्योंकि यह बाइंडिंग और पार्सिंग में अधिक कुशल है। Fish Shell में JSON के साथ काम करने के लिए `jq` सबसे प्रचलित टूल है, लेकिन अन्य उपकरण जैसे `jshon` या `fx` भी हैं। `jq` का उपयोग करके, हम पूरी ताकत और लचीलापन पा सकते हैं जो UNIX शैली के पाइपलाइन और टेक्स्ट प्रोसेसिंग टूल्स के साथ आसानी से इंटीग्रेट हो सकते हैं।

## See Also (यह भी देखें)
- `jq` औपचारिक दस्तावेज़ीकरण: https://stedolan.github.io/jq/manual/
- JSON के बारे में अधिक जानकारी: https://www.json.org/json-en.html
- `jshon` का इस्तेमाल: http://kmkeen.com/jshon/
- `fx` के बारे में: https://github.com/antonmedv/fx
