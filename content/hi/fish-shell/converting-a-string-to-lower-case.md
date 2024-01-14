---
title:    "Fish Shell: स्ट्रिंग को लोअर केस में बदलना"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्यों 
कभी-कभी हमें अपने कोड में दिए गए शब्दों को हल्के रूप में लिखने की जरूरत होती है, ताकि प्रोग्राम को समझने में आसानी हो। स्ट्रिंग को निचली अक्षर के रूप में प्रारूपित करने से यह काम आसान हो जाता है।

## कैसे करें
```Fish Shell
echo "HELLO WORLD" | tr [:upper:] [:lower:]
```
यह कमांड स्ट्रिंग "HELLO WORLD" को "hello world" में बदल देगा। आप भी कोई अन्य स्ट्रिंग उपयोग कर सकते हैं।

## गहराई में डूबना
कभी-कभी हमें स्ट्रिंग को लोअर केस में प्रारूपित करने के लिए स्पेशल कारक्टर्स का उपयोग करना पड़ता है। इसमें हम अलग-अलग तरह के दृश्यानुक्रम को बनाने के लिए सुनिश्चित होना चाहिए और कुछ समय तो हमें इसमें समस्याएं भी हो सकती हैं। लेकिन फिश शेल की एक्‍टूअल क्षमताएं हमें स्ट्रिंग का सम्‍बन्‍ध बनाने और संशोधन करने की सुविधा प्रदान करती हैं।

## देखें भी
- [Fish Shell वेबसाइट](https://fishshell.com/)
- [फिश शेल सोर्स कोड](https://github.com/fish-shell/fish-shell)
- [Fish Shell दस्तावेज़ीकरण](https://fishshell.com/docs/current/index.html)