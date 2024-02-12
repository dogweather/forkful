---
title:                "स्ट्रिंग को जोड़ना"
aliases:
- /hi/fish-shell/concatenating-strings/
date:                  2024-01-20T17:35:14.815632-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग को जोड़ना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
स्ट्रिंग कनकटेनेशन दो या अधिक टेक्स्ट स्ट्रिंग्स को जोड़ना होता है। प्रोग्रामर्स इसे डायनामिक रूप से एक संगत संदेश बनाने या डेटा को फॉर्मेट करने के लिए करते हैं।

## How to: (कैसे करें:)
```Fish Shell
# स्ट्रिंग्स कनकटेनेट करना
set string1 "नमस्ते, "
set string2 "दुनिया!"
set concatenated_string $string1$string2
echo $concatenated_string # आउटपुट: "नमस्ते, दुनिया!"
```

```Fish Shell
# स्पेस के साथ स्ट्रिंग्स जोड़ना
set greet "नमस्ते"
set entity "दुनिया"
echo "$greet, $entity!" # आउटपुट: "नमस्ते, दुनिया!"
```

```Fish Shell
# वेरिएबल और स्ट्रिंग्स का मिश्रण
set user "Amit"
echo "उपयोगकर्ता का नाम है: $user" # आउटपुट: "उपयोगकर्ता का नाम है: Amit"
```

## Deep Dive (गहराई से जानकारी)
Fish Shell में स्ट्रिंग्स को आसानी से जोड़ा जा सकता है, बिना किसी विशेष संयोजक (+ या अन्य) का इस्तेमाल किए। यही नहीं, बल्कि Fish प्रोग्रामिंग की सरलता प्रदान करता है ऐसा करने में, जो उपयोगकर्ता के लिखने का समय बचाता है।

पुराने शेल जैसे Bash में, प्रायः स्ट्रिंग्स जोड़ने के लिए उद्धरण चिह्नों (' या ") का इस्तेमाल होता था। Fish में, यह ज्यादा सहज हो गया है, आप सीधे ही वेरिएबल्स को एक के बाद एक लिख सकते हैं।

विकल्पों में `string` कमांड भी शामिल है जिसके साथ आप विभिन्न स्ट्रिंग ऑपरेशन्स कर सकते हैं, जैसे कि जोड़ना (`string join`), विभाजित करना (`string split`) व अन्य।

## See Also (और भी देखें)
- Fish Documentation: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- Tutorial on String Manipulation in Fish: [https://fishshell.com/docs/current/tutorial.html#tut_strings](https://fishshell.com/docs/current/tutorial.html#tut_strings)
- Fish Shell GitHub Repository: [https://github.com/fish-shell/fish-shell](https://github.com/fish-shell/fish-shell)
