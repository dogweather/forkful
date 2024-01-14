---
title:                "Haskell: कम्प्यूटर प्रोग्रामिंग में कमांड लाइन आर्ग्यूमेंट्स पढ़ना"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## क्यों

कमांड लाइन विकल्पों को पढ़ने का आपत्तिजनक काम हो सकता है, लेकिन यह हैसकेल प्रोग्रामिंग में अत्यंत महत्वपूर्ण होता है। यह बहुत सारी अवस्थाओं में उपयोगी हो सकता है, जैसे कि अधिकतम सुरक्षा के लिए अनुरोध प्रोग्रामों को, विभिन्न प्रकार के समाप्ति और विधि कार्यों को, और आपके कोड का आकंशी पूर्वानुमान करने के लिए।

## कैसे

कमांड लाइन विकल्पों को पढ़ने के लिए हस्केल में `System.Environment.getArgs` फ़ंक्शन का उपयोग किया जाता है। इस फ़ंक्शन को अपने कोड में इस प्रकार से लिखा जा सकता है:

```Haskell
import System.Environment

main = do
  args <- getArgs
  print args
```

यदि आप अपने कोड को आवृत्ति से संपत्ति बनाना चाहते हैं, तो आप मानचित्र योग्यता प्राप्त कमांड लाइन विकल्पों की गणना भी कर सकते हैं। आप इसका उपयोग `System.Console.GetOpt` प्रक्रिया के साथ कर सकते हैं, जो स्टैंडर्ड ऊपर दिए गए कमांड लाइन विकल्पों को देखेगा और उन्हें आपके कोड के लिए उदाहरण जोड़ेंगे। इस प्रक्रिया को आप इस प्रकार से उपयोग में ला सकते हैं:

```Haskell
import System.Console.GetOpt
import System.Environment

data Options = Options {
  optFlag :: Bool,
  optInputFile :: FilePath
} deriving Show

defaultOptions = Options {
  optFlag = False,
  optInputFile = ""
}

options :: [OptDescr (Options -> Options)]
options = [
  Option ['f'] ["flag"]
         (NoArg (\opts -> opts { optFlag = True }))
         "Set flag"
  Option ['i'] ["input"]
         (ReqArg (\file opts -> opts { optInputFile = file }))
         "Input file"
  ]

main = do
  args <- getArgs
  let (opts, nonOpts, errors) = getOpt Permute options args
  case errors of
    [] -> print opts
    _ -> error (concat errors)
```

यहां हमने `Options` डेटा टाइप बनाया और उसम