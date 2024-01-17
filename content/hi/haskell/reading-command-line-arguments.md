---
title:                "कम्प्यूटर प्रोग्रामिंग पर एक लेख का शीर्षक: कमांड लाइन आर्ग्यूमेंट्स को पढ़ना।"
html_title:           "Haskell: कम्प्यूटर प्रोग्रामिंग पर एक लेख का शीर्षक: कमांड लाइन आर्ग्यूमेंट्स को पढ़ना।"
simple_title:         "कम्प्यूटर प्रोग्रामिंग पर एक लेख का शीर्षक: कमांड लाइन आर्ग्यूमेंट्स को पढ़ना।"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## क्या है और क्यों?
कमांड लाइन आर्गुमेंट (command line arguments) वह डाटा है जो एक प्रोग्राम को कमांड लाइन से प्राप्त होता है। हस्केल प्रोग्रामर्स इस्तेमाल कर सकते हैं ताकि उनके प्रोग्राम को स्लाइस (slice) दिया जा सके या अपनाया जा सके। 

## कैसे करें:
```Haskell
import System.Environment

main = do
  args <- getArgs
  putStrLn ("मेरा पहला आर्गुमेंट है: " ++ head args)
```

जब आप इस प्रोग्राम को चलाएंगे, तो आपको एक आर्गुमेंट पास करना होगा। उदाहरण के लिए, यदि आप इस प्रोग्राम को "हेलो" नाम से सेव करेंगे, तो आपको `मेरा पहला आर्गुमेंट है: हेलो` का आउटपुट मिलेगा। 

## गहरा जाँच:
कमांड लाइन आर्गुमेंट पढ़ने के इतिहास पर कुछ गहरी जानकारी नहीं है। हालांकि, हस्केल के साथ, आप मेमोरी से दूर अपने आर्गुमेंट्स को स्टोर करने की अवश्यकता के साथ, अपने कोड को सुव्यवस्थित और साफ रख सकते हैं। अन्य कार्यक्षम भाषाओं में, आप डाटा स्ट्रक्चर्स (data structures) के साथ काम करते हैं ताकि आपको सामान्य डाटा स्ट्रक्चर्स का निर्माण करने की आवश्यकता हो। 

## यह भी देखें:
- [Get command line arguments in Haskell](https://stackoverflow.com/questions/7543733/getting-command-line-arguments-in-haskell)
- [Understanding command line parameters in Haskell](https://stackoverflow.com/questions/49121303/understanding-command-line-parameters-in-haskell)