---
date: 2024-01-20 17:54:04.693553-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u0938\u0948\
  \u0902\u092A\u0932 \u0906\u0909\u091F\u092A\u0941\u091F."
lastmod: '2024-04-05T21:53:54.625407-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u0938\u0948\u0902\u092A\
  \u0932 \u0906\u0909\u091F\u092A\u0941\u091F."
title: "\u091F\u0947\u0915\u094D\u0938\u094D\u091F \u092B\u093C\u093E\u0907\u0932\
  \ \u092A\u0922\u093C\u0928\u093E"
weight: 22
---

## How to: (कैसे करें:)
```Bash
# टेक्स्ट फाइल पढ़ने के लिए cat कमांड

cat myfile.txt

# लाइन-बाय-लाइन पढ़ने के लिए while-loop और read कमांड

while IFS= read -r line; do
    echo "$line"
done < myfile.txt
```

सैंपल आउटपुट:
```
हैलो, यह मेरी फाइल की पहली लाइन है।
दूसरी लाइन यहाँ है।
और यह तीसरी लाइन है।
```

## Deep Dive (गहराई में जानकारी)
पाठ फ़ाइल पढ़ना यूनिक्स-जैसे सिस्टम्स पर बैश के माध्यम से बहुत सरल है, जिसका इतिहास 1970 के दशक तक जाता है। `cat` कमांड सीधा और आसान है, पर बड़ी फाइलों के लिए अव्यावहारिक हो सकता है। `while`-loop मेमोरी प्रभावी होता है जबकि `awk`, `sed` जैसे टूल्स और अधिक जटिल प्रोसेसिंग की अनुमति देते हैं। स्क्रिप्ट में, IFS (Internal Field Separator) का उपयोग field splitting को नियंत्रित करने के लिए होता है। `-r` फ्लैग `read` कमांड में backslash इस्केपिंग को अवरुद्ध करता है।

## See Also (और देखें)
- [GNU Bash manual](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html) - Bash के औपचारिक दस्तावेज़।
- [Advanced Bash-Scripting Guide](https://www.tldp.org/LDP/abs/html/) - जटिल Bash स्क्रिप्टिंग के लिए गाइड।
- [ShellCheck](https://www.shellcheck.net/) - शेल स्क्रिप्ट्स को ऑनलाइन जाँचने के लिए उपकरण।
