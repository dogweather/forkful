---
title:                "एक पाठ फ़ाइल पढ़ना"
html_title:           "Bash: एक पाठ फ़ाइल पढ़ना"
simple_title:         "एक पाठ फ़ाइल पढ़ना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

# हास्केल में टेक्स्ट फ़ाइल पढ़ना

## क्या और क्यों?
टेक्स्ट फ़ाइल पढ़ने का मतलब है किसी फ़ाइल से डेटा लेना। प्रोग्रामर्स इसे इसलिए करते हैं क्योंकि यहाँ से उन्हें उनके प्रोग्राम के लिए आवश्यक डेटा मिलता है।

## कैसे करें:
`readFile` फ़ंक्शन का उपयोग करके हम टेक्स्ट फ़ाइल पढ़ सकते हैं। 

```Haskell 
import System.IO  
   
main = do   
    contents <- readFile "hello.txt"  
    putStr contents
```  
अगर "hello.txt" में "नमस्ते दुनिया" है, तो आउटपुट ऐसा होगा: 

```
नमस्ते दुनिया
```

## गहरा डाइव
हास्केल का इस्तेमाल साल 1990 से हो रहा है। इसका उद्देश्य उन सभी असुविधाओं को दूर करना था जो अन्य भाषाओं में सामान्य रूप से होती थीं। 

`readFile` का विकल्प फ़ंक्शन है `hGetContents`. `hGetContents` हैंडल का उपयोग करता है जो बहुत अधिक आवश्यकताओं को पूरा करने के लिए अधिक नियंत्रण प्रदान कर सकता है। 

`readFile` फ़ंक्शन डेटा को लेने के लिए लेजी तरीका अपनाता है। यह डेटा को आवश्यकता के हिसाब से लेता है, जिससे हमारे सिस्टम की प्रदर्शन क्षमता पर नकारात्मक प्रभाव नहीं पड़ता है। 

## और भी देखें
अधिक जानकारी के लिए, निम्नलिखित लिंक पर क्लिक करें :

[Haskell Text.File Interface](http://hackage.haskell.org/package/base-4.2.0.1/docs/System-IO.html)
[Haskell Wiki](https://wiki.haskell.org/Index)