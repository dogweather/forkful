---
title:                "Haskell: कम्प्यूटर प्रोग्रामन्ग के लिए कमांड लाइन आर्ग्यूमेंट्स पढ़ना"
simple_title:         "कम्प्यूटर प्रोग्रामन्ग के लिए कमांड लाइन आर्ग्यूमेंट्स पढ़ना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## क्यों
अगर आप किसी हैस्केल प्रोग्रामर हैं या बनना चाहते हैं, तो आपको कमांड लाइन एर्ग्यूमेंट को पढ़ना सीखना बहुत जरूरी है। इससे आपको अपने प्रोग्राम में उपयोगकर्ता से दिए गए इनपुट को प्रोसेस करने का तरीका समझने में मदद मिलेगी।

## कैसे करें
```Haskell
import System.Environment

main = do
    args <- getArgs
    putStrLn ("Command line arguments: " ++ show args)
```
ऊपर दिए गए हस्केल कोड में, हमने `System.Environment` मॉड्यूल को इंपोर्ट किया है। यह मॉड्यूल हमें `getArgs` नामक फ़ंक्शन को उपयोग करने का अनुमति देता है, जो हमें सभी कमांड लाइन एर्ग्यूमेंट को स्ट्रिंग लिस्ट के रूप में देता है। इसके बाद, हम `putStrLn` फ़ंक्शन का उपयोग करके उपयोगकर्ता से दिए गए इनपुट को प्रिंट करते हैं। ध्यान दें कि, `show` फ़ंक्शन को उपयोग करके हम स्ट्रिंग लिस्ट को स्ट्रिंग में रूपांतरित करते हैं ताकि हम उसे प्रिंट कर सके।

इसके बाद, आप कमांड लाइन में `runhaskell` कमांड का उपयोग करके अपना हस्केल कोड चला सकते हैं, जिससे आपको उपयोगकर्ता द्वारा दिए गए इनपुट का अंदाज़ा लग सके।

```bash
$ runhaskell command-line-args.hs hello world
Command line arguments: ["hello","world"]
```

## गहराई में जाएं
अगर आप गहराई में जाना चाहते हैं, तो आप हस्केल के `System.Environment` मॉड्यूल को जान सकते हैं। इसमें अन्य भी फ़ंक्शन हैं, जैसे `getProgName` जो हमें वर्तमान प्रोग्राम का