---
title:                "स्ट्रिंग को छोटे अक्षरों में परिवर्तित करना"
date:                  2024-01-20T17:39:44.022596-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग को छोटे अक्षरों में परिवर्तित करना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
स्ट्रिंग को लोअर केस में बदलने का मतलब सभी अक्षरों को छोटे (lowercase) रूप में परिवर्तित करना है। प्रोग्रामर यह इसलिए करते हैं क्योंकि अक्सर डेटा को सामान्य करने और संवेदनशीलता को हटाने (case-insensitive comparisons) के लिए यह जरूरी होता है।

## How to: (कैसे करें:)
Fish Shell में एक स्ट्रिंग को लोअर केस में बदलने के लिए आप `string lower` फंक्शन का उपयोग कर सकते हैं। नीचे कुछ कोड उदाहरण और उनके आउटपुट दिए गए हैं।

```fish
# एक वेरिएबल में स्ट्रिंग को लोअर केस में बदलना
set myString "Namaste Duniya!"
echo $myString | string lower  # आउटपुट: namaste duniya!
```

```fish
# डायरेक्ट स्ट्रिंग इनपुट को लोअर केस में बदलना
string lower "HELLO, How Are You?"  # आउटपुट: hello, how are you?
```

## Deep Dive (गहराई में जानकारी)
String को लोअरकेस में बदलना यूनिक्स-जैसे शेल्स में बहुत सालों से एक मानक कार्य है। `tr`, `awk`, और `sed` जैसे पुराने कमांड-लाइन टूल्स का इस्तेमाल करके इसे किया जाता था, लेकिन Fish में `string` बिल्ट-इन फंक्शन इसे सरल बनाता है।

Fish Shell का `string` फंक्शन, जो Fish 2.3.0 से आया, यह सीधा और व्यवहारिक है। यह यूनिकोड अक्षरों को भी सही से लोअरकेस में बदल देता है, जो कि कुछ पुराने टूल्स में समस्या हो सकती थी। Fish Shell के `string` फंक्शन में और भी कई ऑपरेशन जैसे कि replace, trim, और match शामिल हैं, जो इसे और भी शक्तिशाली बनाते हैं।

## See Also (संबंधित सूत्रों को देखें)
- Fish Shell डॉक्यूमेंटेशन: `string lower` के बारे में और जानकारी के लिए यहाँ जाएँ: [Fish Shell String Documentation](https://fishshell.com/docs/current/cmds/string.html)
- यूनिक्स `tr` कमांड: यदि आप Fish के बजाय पारंपरिक शेल्स में काम कर रहे हैं, तो `tr` कमांड के बारे में यहाँ सीखें: [Unix tr Command](https://man7.org/linux/man-pages/man1/tr.1.html)
- टेक्स्ट प्रोसेसिंग: अधिक जटिल टेक्स्ट मैनीपुलेशन के लिए `awk` और `sed` कमांड्स पर गाइड: [GNU Awk Documentation](https://www.gnu.org/software/gawk/manual/), [Sed Documentation](https://www.gnu.org/software/sed/manual/sed.html)