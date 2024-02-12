---
title:                "टेक्स्ट फ़ाइल पढ़ना"
aliases: - /hi/bash/reading-a-text-file.md
date:                  2024-01-20T17:54:04.693553-07:00
model:                 gpt-4-1106-preview
simple_title:         "टेक्स्ट फ़ाइल पढ़ना"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
पाठ फ़ाइल को पढ़ना कोड के माध्यम से फाइल की सामग्री को एक्सेस करना है। प्रोग्रामर इसका उपयोग कॉन्फ़िगरेशन, डेटा इनपुट, लॉग्स, इत्यादि के लिए करते हैं।

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
