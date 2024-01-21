---
title:                "तारीख को स्ट्रिंग में बदलना"
date:                  2024-01-20T17:36:34.021356-07:00
model:                 gpt-4-1106-preview
simple_title:         "तारीख को स्ट्रिंग में बदलना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

तारीख को स्ट्रिंग में बदलने का मतलब उसे टेक्स्ट फॉर्मेट में बदलना है, जिससे वह पढ़ने में आसान और शेयर करने में सरल हो जाए। प्रोग्रामर्स इसे लॉग्स, यूजर इंटरफेस और डाटा स्टोर करते वक्त करते हैं।

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