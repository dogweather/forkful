---
title:                "यादृच्छिक संख्याएँ उत्पन्न करना"
date:                  2024-01-20T17:49:03.672088-07:00
model:                 gpt-4-1106-preview
simple_title:         "यादृच्छिक संख्याएँ उत्पन्न करना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
रैंडम नंबर्स का उत्पादन एक प्रक्रिया है जिसमें नंबर्स का सेट पैदाहोता है जो किसी पैटर्न का पालन नहीं करते। प्रोग्रामर्स इसे टेस्टिंग, सिक्युरिटी और गेम डिज़ाइन में उपयोग करते हैं।

## How to: (कैसे?)
Fish Shell में रैंडम नंबर जेनरेट करना बहुत सरल है। `random` फंक्शन का इस्तेमाल करें:

```Fish Shell
# एक रेंज में रैंडम नंबर जनरेट करें (1 से 100)
set number (random 1 100)
echo $number
```

सैम्पल आउटपुट:
```
42
```

## Deep Dive (गहराई में जानकारी)
Fish Shell में `random` कमांड 2.x संस्करण से मौजूद है और यह Pseudo-random numbers उत्पन्न करता है, जो कि पूरी तरह से अनुमानीत नहीं होते, पर साधारण उपयोग के लिए पर्याप्त होते हैं। और भी तरीके हैं जैसे कि लिनक्स सिस्टम पर `/dev/urandom` का इस्तेमाल करना, जो कि अधिक सुरक्षित माने जाते हैं। Fish Shell का `random` फंक्शन आंतरिक रूप से इसी का इस्तेमाल करता है।

## See Also (और भी देखें)
- Fish Shell Documentation on Random: https://fishshell.com/docs/current/cmds/random.html
- Wikipedia on Pseudo-Random Number Generators (PRNG): https://en.wikipedia.org/wiki/Pseudorandom_number_generator
- Random Number Generation in Computer Science: https://www.sciencedirect.com/science/article/pii/S0022000097915348
