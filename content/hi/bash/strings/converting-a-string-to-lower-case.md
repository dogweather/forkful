---
title:                "स्ट्रिंग को छोटे अक्षरों में परिवर्तित करना"
aliases:
- hi/bash/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:37:53.333449-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग को छोटे अक्षरों में परिवर्तित करना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
स्ट्रिंग को लोअर केस में बदलना मतलब उसमें मौजूद सारे अक्षरों को छोटे अक्षरों में परिवर्तित करना। प्रोग्रामर इसे तब करते हैं जब case sensitivity के कारणों से तुलना या डाटा संग्रह करना होता है।

## How to: (कैसे करें)
```Bash
# वेरिएबल में स्ट्रिंग को लोअर केस में सेट करना
my_string="Hello World!"
lowercase_string=$(echo "$my_string" | tr '[:upper:]' '[:lower:]')
echo $lowercase_string  # hello world!

# Bash 4.0+ का उपयोग करना
declare -l lowercase_string2="Hello World Again!"
echo $lowercase_string2  # hello world again!
```

## Deep Dive (गहराई से समझिए)
जब लिनक्स और यूनिक्स सिस्टम में शेल स्क्रिप्टिंग आरम्भ हुई थी, तब `tr` या "translate characters" उपकरण का इस्तेमाल होता था। यह यूनिक्स की प्रारंभिक features में से एक है। बाश वर्जन 4.0+ से, डेवलपर्स को case manipulation के लिए built-in support मिली, जिससे पहले के तरीके (`tr`, `awk`, etc.) को अब उतना इस्तेमाल नहीं किया जाता। `declare` statement के `-l` और `-u` options से क्रमश: lowercase और uppercase में बदलाव कर सकते हैं।

## See Also (और जानकारी के लिए)
- Bash Manual: [https://www.gnu.org/software/bash/manual/](https://www.gnu.org/software/bash/manual/)
- Advanced Bash-Scripting Guide: [https://tldp.org/LDP/abs/html/](https://tldp.org/LDP/abs/html/)
- `tr` Command Tutorial: [https://www.geeksforgeeks.org/tr-command-in-unix-linux-with-examples/](https://www.geeksforgeeks.org/tr-command-in-unix-linux-with-examples/)
- Stack Overflow discussion on string manipulation: [https://stackoverflow.com/questions/tagged/string+manipulation+bash](https://stackoverflow.com/questions/tagged/string+manipulation+bash)
