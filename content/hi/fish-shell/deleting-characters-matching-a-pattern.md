---
title:                "पैटर्न से मिलते जुलते वर्णों को हटाना"
html_title:           "Elixir: पैटर्न से मिलते जुलते वर्णों को हटाना"
simple_title:         "पैटर्न से मिलते जुलते वर्णों को हटाना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
जो भी शब्द संगति से मेल खाते हैं, उन्हें मिटाना एक सामान्य प्रोग्रामिंग टैस्क है। किसी शब्द, संख्या, या अनुच्छेद को खोजना और उसे मिटाना या बदलना यदि वह हमारे कोड के संगत नियमों से मेल खाता है।

## कैसे:
फिॅश शेल में, हम 'string match' और 'string replace' कमांड का उपयोग करके शब्दों को मिटा सकते हैं:

```Fish Shell
set string "I love programming"
echo $string | string match -r -v "programming"
```
और इसका परिणाम होगा:

```Fish Shell
"I love")
```
अगर हम किसी शब्द को बदलना चाहते हैं तो:
```Fish Shell
echo $string | string replace "programming" "coding"
```
और इसका परिणाम होगा:

```Fish Shell
"I love coding"
```
## गहराई की ओर:
फ़िश शेल, किसी विशेष शब्द को मिटाने या बदलने का एक शक्तिशाली तरीका प्रदान करती है। भाग्यशाली रूप से, 'string match' और 'string replace' संरचनाएं ईर्द-गिर्द कैरेट सैशन में उपयोग होने वाले प्रमुख विधियों में से हैं। 'awk' और 'sed' जैसे उपकरण भी उपलब्ध हैं, लेकिन फ़िश चयन करने के लिए एक मार्गदर्शक भूमिका निभाती है, क्योंकि यह प्रस्तावित ढंग से काम करती है, यदि आप इसे अपेक्षित तरीके से उपयोग करते हैं।

## और देखें:
निम्नलिखित स्रोत फ़िश शेल के अन्य मार्गदर्शन का प्रदान करते हैं:
1. [रशमियाँ] (https://fishshell.com/docs/current/index.html)
2. [शब्द संगति] (https://fishshell.com/docs/current/cmds/string-match.html)
3. [शब्द बदले] (https://fishshell.com/docs/current/cmds/string-replace.html)