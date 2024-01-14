---
title:                "Bash: एक पैटर्न के समान अक्षर हटाना"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## क्यों

बाश प्रोग्रामिंग एक बहुत ही उपयोगी और शक्तिशाली तरीका है ऑटोमेशन को प्रदर्शित करने के लिए। एक ऐसी चीज़ है कि डेलीट के दौरान, हमारे कार्य को ध्यान से रखने, जो सामान्य लगता है लेकिन प्रत्येक बार इसे स्वतंत्र रूप से करना मुश्किल हो सकता है। इसलिए, हम आयत दिया है जिसमें एक निश्चित पैटर्न का कोई मिलता है।

## कैसे

```Bash
# डेलीट कॉमांड लिखो।
delete matching pattern
```

उपरोक्त अनुभाग में, हमने बाश रिक्ति का उपयोग करके एक मैचिंग पैटर्न के साथ डिलीट कमांड को लिखा है। यह ऐसी प्रक्रिया हो सकती है:

```Bash
# स्ट्रिंग दिए गए हैं।
string="hello 123 world"

# स्ट्रिंग से पैटर्न को निकालो।
delete matching pattern

# पैटर्न के अनुसार विशेष करके स्ट्रिंग जाँचो।
echo $string

Output: hello world
```

यहां, हमने स्ट्रिंग "hello 123 world" को दिया है और उससे "123" को हटाया है। आप अपने प्रत्येक डेलीट cause में रूपांतरण के लिए इस तरीके का उपयोग कर सकते हैं।

## डीप डाइव

हमने अभी तक डिलीट किए गए कारणों के बारे में अधिकतम जानकारी दी है, लेकिन डीप डाइव में, मैं आपको बाश rृामेंटोरनर का उपयोग करके स्पष्ट कर सकता हूं:

- The -d option can be used to specify the character or characters to be deleted. For example, "-d 1" will delete all "1" characters.
- The -f option can be used to specify the fields on which the pattern matching has to be applied. For example, "-f 2-4" will delete pattern only in fields 2 to 4.
- The -s option can be used to specify the separator between fields. For example, "-s ' '" will use a space as the separator between fields.
- The -i option can be