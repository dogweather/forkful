---
title:                "यादृच्छिक संख्याओं का निर्माण"
html_title:           "Clojure: यादृच्छिक संख्याओं का निर्माण"
simple_title:         "यादृच्छिक संख्याओं का निर्माण"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

रैंडम नंबर उत्पन्न करना मतलब कम्प्यूटर द्वारा अनुमानित नंबर का चयन करना। एक प्रोग्रामर जब इसे करता है, तो वह आमतौर पर परीक्षण, डेटा उत्पादन, और एडवांस्ड एल्गोरिदम के लिए एक 'अनपेक्षित' परिणाम का उपयोग कर रहा होता है।

## कैसे करें:

Fish Shell में, हम रैंडम नंबर उत्पन्न करने के लिए `random` कमांड का उपऔयग कर सकते हैं।

```fish
echo (random 1 100)
```

यह कमांड 1 से 100 के बीच का एक रैंडम नंबर उत्पन्न करेगा। 

## गहरा अध्ययन

रैंडम नंबर जनरेशन का इतिहास गणना की दुनिया में गहरा है। यह पहली बार न्यायिक प्रमाण, गणितीय समस्याओं के हल, और गेम प्ले में उपयोग किया गया था। Fish Shell में रैंडम नंबर जनरेशन `random` कमांड के माध्यम से किया जाता है जो GNU coreutils आधारित होता है। अन्य स्क्रिप्टिंग भाषाओं में जैसे Python, JavaScript का भी रैंडम नंबर जनरेटर उपलब्ध हैं। 

## अन्य जानकारी:

Fish Shell द्वारा जनरेट किए गए रैंडम नंबर के बारे में अधिक जानने के लिए, नीचे दिए गए स्रोतों का अन्वेषण करें:

- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [Deep Dive into Random Number Generation](https://www.shadertoy.com/view/lsB3zK)
- [GNU Coreutils on Random](https://www.gnu.org/software/coreutils/manual/coreutils.html#Random-sources)