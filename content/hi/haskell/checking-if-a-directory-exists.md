---
title:                "डायरेक्टरी के अस्तित्व की जांच"
html_title:           "Haskell: डायरेक्टरी के अस्तित्व की जांच"
simple_title:         "डायरेक्टरी के अस्तित्व की जांच"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## क्यों

क्या आपने कभी सोचा है कि कैसे हैस्केल में फ़ोल्डर के अस्तित्व को जाँचने का कोड लिखा जा सकता है? यह आपको अपने प्रोग्राम में अतिरिक्त सुरक्षा और त्रुटि निवारण की दृष्टि से मदद कर सकता है।

## कैसे करें

आप आसानी से `System.Directory` मॉड्यूल के `doesDirectoryExist` फ़ंक्शन का उपयोग करके फ़ोल्डर के अस्तित्व को जाँच सकते हैं। यह फ़ंक्शन आपको `True` या `False` के रूप में परिणाम देगा। नीचे दिए गए कोड ब्लॉक में आप इस फ़ंक्शन का उपयोग करते हुए एक सादा उदाहरण देख सकते हैं।

```Haskell
import System.Directory

main = do
  isExist <- doesDirectoryExist "myFolder"
  if isExist
    then putStrLn "myFolder exists!"
    else putStrLn "myFolder doesn't exist."
```

यदि आपके सिस्टम में `myFolder` फ़ोल्डर है, तो आप उपरोक्त कोड का उपयोग करके उपयोगकर्ता को `myFolder exists!` संदेश देखेंगे। यदि यह फ़ोल्डर मौजूद नहीं है, तो आप `myFolder doesn't exist.` संदेश देखेंगे।

## गहराई में जाएं

जब हम `doesDirectoryExist` फ़ंक्शन का उपयोग करते हैं, तो हम वास्तविक में सिस्टम कमांड `unix find` का उपयोग कर रहे होते हैं। हाईस्केल कोड के पीछे की प्रक्रिया गहराई में समझने के लिए, आप `System.Directory` मॉड्यूल के स्रोत को जांच सकते हैं। मॉड्यूल में `unix find` का कोड सीधे बारह (`dozen`) लाइन कोड होता है जो `libc` से सीधा जुड़ता है। यह सूचक है कि हम अपने वचन को साज़िशी अनुमोदन के साथ लिख रहे है