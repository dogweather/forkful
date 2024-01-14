---
title:                "Fish Shell: एक स्ट्रिंग की लंबाई ढूंढना।"
simple_title:         "एक स्ट्रिंग की लंबाई ढूंढना।"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## क्यों

किसी चुटकी भर कोड से हम तामसील चीज़ों को कंप्यूटर के साथ काम करने में आसानी से समझ सकते हैं जैसे की शब्दों को और ज्यादा सटीकता के साथ देखने में मूल्यपूर्ण है।

## कैसे

```Fish Shell में एक बार में सिर्फ एक लाइन इस्तेमाल करके हम चुकंदर का आकार निकाल सकते हैं:```

` set length (count $string) `

उपरोक्त कोड में, हम `$string` वेरिएबल में से संख्या लेकर, जो कि आइटम की संख्या देता है, `$length` वेरिएबल में सेट करते हैं।

अगर हमें स्ट्रिंग की लंबाई को प्रिंट करना चाहते हैं, तो हम निम्नलिखित कोड का उपयोग कर सकते हैं:

`echo $length`

एक और उदाहरण दिया जा रहा है जहां हम अन्य आइटमों को इस्तेमाल कर सकते हैं, जैसे कि `for` लूप, ताकि हम एक स्ट्रिंग एरे बना सकें और उसकी आकार को प्रिंट कर सकें:

```
set items golang java python
for item in $items
    set length (count $item)
    echo "The length of $item is $length"
end
```

ऊपरोक्त कोड का आउटपुट इस प्रकार होगा:

```The length of golang is 6
The length of java is 4
The length of python is 6```

## डीप डाइव

Fish Shell में स्ट्रिंग लंबाई के लिए बिल्ट-इन `count` फंक्शन के अलावा भी कई अन्य तरीके हैं। आप `wc` या `grep` कमांड को भी इस्तेमाल कर सकते हैं ताकि आप अपने सरल कोड को और भी बेहतर बना सकें।

## देखें भी

- [Fish Shell documentation](https://fishshell.com/docs/current/index.html)
- [Fish Shell tutorials](https://fishshell.com/docs/current/tutorial.html)
- [Using strings in Fish Shell](https://fishshell.com/docs/current/tutorial.html#using-strings)