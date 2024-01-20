---
title:                "एक स्ट्रिंग से तारीख पार्स करना"
html_title:           "C++: एक स्ट्रिंग से तारीख पार्स करना"
simple_title:         "एक स्ट्रिंग से तारीख पार्स करना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों? 

पार्सिंग एक तारीख का अर्थ होता है- किसी वाक्यांश को प्रक्रिया करना, ताकि कंप्यूटर उसे पढ़ सके। प्रोग्रामर इसे तभी करते हैं जब उन्हें डेटा को उसके मूल फॉर्मेट से बदलकर और व्यावहारिक फॉर्मेट में लाना होता है।

## कैसे करें:

```Fish Shell 
# दिनांक पार्स करने के लिए, Fish में अगला कोड उपयोग करें।
set date_string "2022-10-01"
set parsed_date (date -u -j -f "%Y-%m-%d" $date_string "+%d-%m-%Y")

# पार्स की गई तारीख को मुद्रित करें
echo $parsed_date
```
सैंपल आउटपुट: 
``` 
01-10-2022
```

## गहरा डाइव:

1. ऐतिहासिक प्रसंग: डेटा पार्सिंग का इतिहास एक्साइटिंग और चुनौती पूर्ण है। Fish Shell, जो एक UNIX shell है, पीढ़ियों तक की पारंपरिक shell scripting की असुविधाओं को दूर करने के लिए बनाई गई है।

2. विकल्प: Fish की तुलना में Bash और Zsh जैसे अन्य shells पार्सिंग विभिन्न गुणों के साथ करते हैं। 

3. क्रियान्वयन विशेषताएं: Fish में डेट स्ट्रिंग का पार्स डेताइम ऑब्जेक्ट में किया जाता है, जिसे बाद में तारीख, समय, और टाइमज़ोन की जानकारी प्राप्त करने के लिए उपयोग किया जा सकता है। 

## यह भी देखें:

- [[Fish Shell डॉक्यूमेंटेशन]](https://fishshell.com/docs/current/index.html)
- [[Fish Shell GitHub]](https://github.com/fish-shell/fish-shell)