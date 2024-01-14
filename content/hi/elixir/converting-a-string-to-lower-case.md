---
title:    "Elixir: स्ट्रिंग को लोअर केस में रूपांतरण करना"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

Elixir में स्ट्रिंग को लोअर केस में कन्वर्ट करने का कारण:
कोई व्यक्ति स्ट्रिंग को लोअर केस में कन्वर्ट करने के कारण इसलिए भी हो सकता है क्योंकि उन्हें स्ट्रिंग के अंदर की सभी वर्णों को समान ढंग से प्रकट करना होता है।

इसके लिए कैसे:
कोई भी स्ट्रिंग को लोअर केस में कन्वर्ट करने के लिए आपको String.downcase / 1 फंक्शन का उपयोग करना होगा। इसके लिए पहले आपको आपके स्ट्रिंग का नामेंग्स फंक्शन का उपयोग करना होगा, उसके बाद इसे downcase करना होगा। नीचे दिए गए कोड ब्लॉक में आप इसका एक उदाहरण देख सकते हैं।

```
इनपुट: String.downcase ("HELLO")
आउटपुट: "hello"
```

गहराई में जाएँ:
स्ट्रिंग को लोअर केस में कन्वर्ट करने के लिए आपको स्ट्रिंग फंक्शन के साथ अन्य फंक्शन का उपयोग भी कर सकते हैं, जैसे कि capitalize, swapcase, आदि। इसलिए, आपको अपनी स्ट्रिंग का एक्सप्लोर करना चाहिए जिससे कि आप गनों से उसे लोअर केस में कन्वर्ट करने के अलावा अन्य विकल्पों का भी उपयोग कर सकें।

अधिक जानकारी के लिए:
https://hexdocs.pm/elixir/String.html#downcase/2

देखें भी:
आगे आप विभिन्न तरीकों से इंटरनेट पर स्ट्रिंग को लोअर केस में कन्वर्ट करने के लिए मौजूद लिंक्स देख सकते हैं:

1. https://www.techbeamers.com/powershell-string-lower-case-convert/
2. https://www.delftstack.com/howto/python/python-change-string-to-lowercase/
3. https://tecadmin.net/bash-convert-string-lowercase/

## देखें भी:

https://hexdocs.pm/elixir/String.html
https://www.tutorialspoint.com/elixir/