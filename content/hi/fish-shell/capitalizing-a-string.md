---
title:                "शब्दस्त्रोत सही रूप में लिखना"
html_title:           "Fish Shell: शब्दस्त्रोत सही रूप में लिखना"
simple_title:         "शब्दस्त्रोत सही रूप में लिखना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Fish Shell में स्ट्रिंग कैपिटलाइज़ करना

## क्या और क्यों?
स्ट्रिंग कैपिटलाइज़ करना एक तरीका है जिससे हमारे कंप्यूटर प्रोग्राम में एक शब्द को समझने के लिए पहले अक्षर को बड़ा किया जाता है। ऐसा करने से हमारे प्रोग्राम को आसान बनाया जाता है क्योंकि इससे शब्द को पहचानने में दिक्कत नहीं होती है।

## कैसे?
```Fish Shell
set first_name "john"
echo $first_name
```
आउटपुट: john

```Fish Shell
set first_name (string capitalize "john")
echo $first_name
```
आउटपुट: John

## गहराई में
स्ट्रिंग कैपिटलाइज़ करना प्रोग्रामिंग में बहुत ही उपयोगी है, क्योंकि इससे शब्दों को पहचानने में आसानी होती है। पहले इसका इस्तेमाल यूनिक्स ऑपरेटिंग सिस्टम में बड़े शब्दों को पहचानने के लिए किया जाता था। अब भी कई अन्य प्रोग्रामिंग भाषाओं और शेल में भी इसका इस्तेमाल किया जाता है।

## और भी देखें
- [String capitalize फ़ंक्शन के बारे में और जानें](https://fishshell.com/docs/current/cmds/capitalize.html)
- [यूनिक्स ऑपरेटिंग सिस्टम के इतिहास के बारे में और जानें](https://en.wikipedia.org/wiki/History_of_Unix)
- [कंप्यूटर प्रोग्रामिंग भाषाओं के बारे में और जानें](https://en.wikipedia.org/wiki/List_of_programming_languages)