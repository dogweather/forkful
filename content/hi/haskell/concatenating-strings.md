---
title:                "Haskell: स्ट्रिंग्स को जोड़ना"
simple_title:         "स्ट्रिंग्स को जोड़ना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## क्यों

कभी-कभी हैस्केल में तुलनात्मक अध्ययन के दौरान आपको एक वर्ग के अन्य ऑब्जेक्ट्स की सूची में समारोह करना पड़ता है। यहाँ आप एक सबसे आसान और अधिक संगठित तरीके से अन्यथा फोर्म में चीजें जोड़ने का स्तर तलाश सकते हैं।

## कैसे करें

```Haskell
concatString :: String -> String -> String
concatString str1 str2 = str1 ++ str2

result = concatString "नमस्ते" "दुनिया"
print result
```

```
नमस्ते दुनिया
```

यहाँ हमने दो स्ट्रिंग्स `str1` और `str2` को संयोजित किया है और इन्हें `concatString` फ़ंक्शन के द्वारा लौटाया है। हमने फ़ंक्शन को `String` तकरीबन के प्रकार के आर्गुमेंट दिए हैं और `++` ऑपरेटर से जोड़ा है।

## गहराई तक

यह बहुत साधारण सा लगता है, लेकिन अधिक समझने के लिए हम इसे थोड़े से गहराई तक जान सकते हैं। हैस्केल में `++` ऑपरेटर दो स्ट्रिंग्स को मिलाता है जो कि एक नए स्ट्रिंग बनाता है। यह काफी सरल है लेकिन दोनों स्ट्रिंग्स के फिर से आमल करने के कारण यह थोड़ा समय लगाएगा। इसलिए, हम इसे एक ही स्ट्रिंग में जोड़ने का `concat` फ़ंक्शन उपलब्ध कराने की कोशिश कर सकते हैं। इससे हमारा प्रोग्राम अधिक अनुकूलित हो सकता है।

## देखें भी

- [Haskell Strings](https://www.tutorialspoint.com/haskell/haskell_strings.htm)
- [Haskell String Functions](https://www.programiz.com/haskell-programming/string-functions)