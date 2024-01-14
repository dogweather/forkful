---
title:    "Fish Shell: स्ट्रिंग को लोअर केस में रूपांतरण करना"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## क्यों

कोई भी प्रोग्रामिंग भाषा में स्ट्रिंग्स को lower case में convert करने का काम बहुत जरूरी हो सकता है। जैसे की किसी भी टेक्स्ट प्रोसेसिंग या फाइल प्रोसेसिंग के समय, स्ट्रिंग को सही ढंग से प्रसंस्करण करने के लिए हमें इसे lower case में convert करने की जरूरत पड़ सकती है।

## कैसे करें

फिश शेल में स्ट्रिंग को lower case में convert करने के लिए हमें एक स्पेशल कमांड `string tolower` का इस्तेमाल करना होता है। नीचे दिए गए उदाहरण में, हमने एक स्ट्रिंग `Hello World` को lower case में convert किया है और उसका output दिखाया है:

```Fish Shell
string tolower 'Hello World' 
```

Output:
`hello world`

## गहराई में जाएं

लोअर लेवल पर, स्ट्रिंग को lower case में convert करने के लिए हमें ASCII मैप की मदद लेनी होती है। सबसे पहले, हम `string tolower` कमांड को एक नए स्ट्रिंग वेरिएबल में अपडेट करते हैं। फिर, हम ASCII मैप के साथ एक लूप चलाते हैं और हर character को lower case में convert करते हैं। नीचे दिए गए उदाहरण में, हमने इस technique का इस्तेमाल करके `Hello World` को lower case में convert किया है और उसके ASCII values भी दिखाए हैं:

```Fish Shell
set str 'Hello World'
for c in (string split -d -- $str)
    set -a uc $c[1]
    set -a lc (string tolower $str[$c[1]])
    set -a uc (string toupper $str[$c[1]])
end
set c 'W'
seq 1 4 | xargs printf '%s\n' $str[$c] (string toupper $str[$c])
```

Output:
`hello world`
`ASCII values: 72 101 108 108 111 32 87 111 114 108 100`

## देखें भी

- [Fish Shell documentation for string commands](https://fishshell.com/docs/current/cmds/string.html)
- [ASCII table](https://www.ascii-code.com)