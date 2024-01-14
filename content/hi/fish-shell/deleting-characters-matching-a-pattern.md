---
title:    "Fish Shell: पैटर्न से मेल खाते हुए अक्षरों को हटाना"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## क्यों

अगर आप किसी जादूसी स्ट्रिंग वाले फ़ाइल से कई गलत चरित्र हटाना चाहते हैं तो आप इस पोस्ट को जारी रखें।

## कैसे करें

```Fish Shell
set mystring "hello123bye456"
echo $mystring
hello123bye456

string delete --regex [0-9]+ $mystring
echo $mystring
hellobye
```

## गहराई तक जाने

इस कोमांड में हम एक जादूसी स्ट्रिंग का उपयोग करते हैं जो काम को आसान बनाती है। हम किसी भी पैटर्न को डिलीट कर सकते हैं जैसे की नंबर्स, ध्वनि, या कोई भी अन्य रूप है। यह कमांड केवल दो चीजों की आवश्यकता है, पैटर्न और उस फ़ाइल या स्ट्रिंग पर जिसे आप यह काम करना चाहते हैं।

## देखें भी

- [Fish Shell documentation](https://fishshell.com/docs/current/cmds/string.html#string-delete)
- [Understanding Regular Expressions in Hindi](https://www.javascripture.com/Regular%20Expressions)
- [Learn the Basics of Fish Shell in Hindi](https://www.guru99.com/fish-shell-tutorial.html)