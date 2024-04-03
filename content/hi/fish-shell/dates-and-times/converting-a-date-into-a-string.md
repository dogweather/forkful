---
date: 2024-01-20 17:36:34.021356-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Fish Shell\
  \ \u092E\u0947\u0902 \u0924\u093E\u0930\u0940\u0916 \u0915\u094B \u0938\u094D\u091F\
  \u094D\u0930\u093F\u0902\u0917 \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\u0947\
  \ \u0915\u0947 \u0909\u0926\u093E\u0939\u0930\u0923."
lastmod: '2024-03-13T22:44:53.086434-06:00'
model: gpt-4-1106-preview
summary: "Fish Shell \u092E\u0947\u0902 \u0924\u093E\u0930\u0940\u0916 \u0915\u094B\
  \ \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u092E\u0947\u0902 \u092C\u0926\
  \u0932\u0928\u0947 \u0915\u0947 \u0909\u0926\u093E\u0939\u0930\u0923."
title: "\u0924\u093E\u0930\u0940\u0916 \u0915\u094B \u0938\u094D\u091F\u094D\u0930\
  \u093F\u0902\u0917 \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\u093E"
weight: 28
---

## How to: (कैसे करें:)
Fish Shell में तारीख को स्ट्रिंग में बदलने के उदाहरण:

```Fish Shell
# वर्तमान तारीख को YYYY-MM-DD फॉर्मेट में प्रिंट करें
set date_string (date "+%Y-%m-%d")
echo $date_string
```

Output सैंपल:
```
2023-03-15
```

## Deep Dive (गहराई में जानकारी):
तारीख को स्ट्रिंग में बदलने की प्रक्रिया में `date` कमांड का इस्तेमाल होता है, जो UNIX सिस्टम से आई है। Fish Shell, `date` को नए तरीके से इस्तेमाल करता है, जैसे कि `strftime` फंक्शन की जगह। इसके विकल्प में `python` या `awk` कमांड भी उपयोग किए जा सकते हैं। इम्प्लीमेंटेशन की बात करें तो, विभिन्न फॉर्मेट ऑप्शन्स (`%Y` के लिए साल, `%m` के लिए महीना, और `%d` के लिए दिन) का इस्तेमाल करते हुए आप तारीख को अलग-अलग फॉर्मेट्स में बदल सकते हैं।

## See Also (देखें भी):
- Fish Shell के डॉक्यूमेंटेशन: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- `date` कमांड के फॉर्मेट ऑप्शन्स: [http://man7.org/linux/man-pages/man1/date.1.html](http://man7.org/linux/man-pages/man1/date.1.html)
- अन्य शेल स्क्रिप्टिंग टुटोरियल: [https://www.shellscript.sh/](https://www.shellscript.sh/)
