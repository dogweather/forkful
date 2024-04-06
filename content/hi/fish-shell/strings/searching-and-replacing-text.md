---
date: 2024-01-20 17:58:24.730163-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902? (How to:) Fish Shell\
  \ \u092E\u0947\u0902 \u091F\u0947\u0915\u094D\u0938\u094D\u091F \u0916\u094B\u091C\
  \u0928\u0947 \u0914\u0930 \u092C\u0926\u0932\u0928\u0947 \u0915\u0947 \u0932\u093F\
  \u090F `string` \u0915\u092E\u093E\u0902\u0921 \u0915\u093E \u0907\u0938\u094D\u0924\
  \u0947\u092E\u093E\u0932 \u0939\u094B\u0924\u093E \u0939\u0948."
lastmod: '2024-04-05T21:53:54.978832-06:00'
model: gpt-4-1106-preview
summary: "(How to:) Fish Shell \u092E\u0947\u0902 \u091F\u0947\u0915\u094D\u0938\u094D\
  \u091F \u0916\u094B\u091C\u0928\u0947 \u0914\u0930 \u092C\u0926\u0932\u0928\u0947\
  \ \u0915\u0947 \u0932\u093F\u090F `string` \u0915\u092E\u093E\u0902\u0921 \u0915\
  \u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0939\u094B\u0924\u093E\
  \ \u0939\u0948."
title: "\u092A\u093E\u0920 \u0916\u094B\u091C\u0928\u093E \u0914\u0930 \u092C\u0926\
  \u0932\u0928\u093E"
weight: 10
---

## कैसे करें? (How to:)
Fish Shell में टेक्स्ट खोजने और बदलने के लिए `string` कमांड का इस्तेमाल होता है:

```Fish Shell
# मूल स्ट्रिंग बनाएँ.
set original "मैं Fish Shell सीख रहा हूँ।"

# 'सीख' को 'इस्तेमाल' से बदलें.
echo $original | string replace "सीख" "इस्तेमाल"

# आउटपुट: मैं Fish Shell इस्तेमाल रहा हूँ।
```

आगे, आपको एक फाइल में टेक्स्ट बदलने की जरूरत हो सकती है:

```Fish Shell
# 'example.txt' फाइल में हर 'Fish' को 'हिंदी Fish' से बदलें.
string replace -a "Fish" "हिंदी Fish" < example.txt > temp.txt; and mv temp.txt example.txt
```

## गहरी जानकारी (Deep Dive)
Fish Shell की `string` कमांड 2015 में रिलीज Fish 2.3.0 में आई थी। पहले, टेक्स्ट खोजने/बदलने के लिए `sed` और `awk` जैसे यूनिक्स उपकरणों पर निर्भर थे। `string` इस्तेमाल करने का फायदा इसकी सादगी और स्पष्टता में है। इसमें नियमित अभिव्यक्तियों (regexes) का समर्थन भी है जो कि जटिल पैटर्न के साथ भी खोजने/बदलने को सक्षम बनाता है।

## संबंधित स्रोत (See Also)
- Fish Shell डॉक्यूमेंटेशन: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- RegEx testing: [https://regex101.com/](https://regex101.com/)
- स्ट्रिंग ऑपरेशंस ट्यूटोरियल: [https://fishshell.com/docs/current/commands.html#string](https://fishshell.com/docs/current/commands.html#string)
