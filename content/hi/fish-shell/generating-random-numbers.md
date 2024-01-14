---
title:                "Fish Shell: संयुक्त रांदिअम नंबर बनाना"
programming_language: "Fish Shell"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

##इसका क्या करने के लिए: एक साधारण समस्या है, जहां हमें यादृच्छिक नंबरों की आवश्यकता होती है, जैसे खेलों को एकाधिकरण करने के लिए।

##कैसे करें: कुछ मामूली Fish Shell संगणनाओं का उपयोग करके एक अद्भुत कोड संग्रह।

```Fish Shell
set -q RANDOM
if not set -q RANDOM; set RANDOM 0 
for i in (seq 1 5)
  echo $RANDOM
end
```
नतीजा:
27403
56450
30741
57548
22469

##गहराई तक: यादृच्छिक संख्याओं को उत्पन्न करने के लिए विभाजन मानचित्रों, क्रिप्टोग्राफी में उपयोग के साथ, और विभिन्न आवेदनों के लिए डिजिटल सुरक्षा में उपयोग करने का तरीका बताया गया है।

## देखें भी: 
- [Fish Shell डॉक्यूमेंटेशन] (https://fishshell.com/docs/current/index.html)
- [कैसे Fish Shell सामान्य संगणनाओं को उपयोग करता है] (https://stackoverflow.com/questions/52566243/how-fish-shell-uses-general-arithmetic)
- [ऐसा ही एक खेल जो जादू संख्याओं का उपयोग करता है] (https://www.youtube.com/watch?v=4LWBLY_Iq-o)

##अब आप यादृच्छिक संख्याएं उत्पन्न करने के लिए Fish Shell का उपयोग कर सकते हैं! धन्यवाद।