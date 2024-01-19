---
title:                "कमांड लाइन तर्कों को पढ़ना"
html_title:           "Kotlin: कमांड लाइन तर्कों को पढ़ना"
simple_title:         "कमांड लाइन तर्कों को पढ़ना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Haskell में कमांड लाइन आर्गुमेंट्स पढ़ना 

## क्या और क्यों?

कमांड लाइन आर्गुमेंट्स पढ़ना, इसका मतलब होता है प्रोग्राम में उपयोगकर्ता के द्वारा दी गयी माहिती को हासिल करना। विकासकर्ताओं को यह सुनिश्चित करने के लिए की उनका प्रोग्राम विभिन्न तरीकों से विन्यस्त किया जा सकता है, इसलिए वे इसे करते हैं।

## कैसे:

```Haskell
मुख्य मोड्यूल (main module) में, हम कमांड लाइन आर्गुमेंट्स के लिए `getArgs` फंक्शन का उपयोग करेंगे:

```Haskell
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    putStrLn ("Hello, " ++ args !! 0)
```

आप किसी विशेष आर्गुमेंट को इंडेक्स (`args !! 0`) के माध्यम से पहुंच सकते हैं। इस उदाहरण में, यह प्रथम आर्गुमेंट ले रहा है।
```

## गहराई से जानकारी

- **ऐतिहासिक प्रासंगिकता**: Haskell भाषा में `getArgs` फ़ंक्शन का इस्तेमाल कमांड लाइन आर्गुमेंट्स को पढ़ने के लिए वाणिज्यिक रूप से कार्य करने के लिए बहुत समय से किया जा रहा है। यह एक मानकीकृत तरीका है।
- **विकल्प**: आप `optparse-applicative` जैसे पैकेज का उपयोग करके भी अधिक विस्तृत और सहायक कमांड लाइन इंटरफ़ेस बना सकते हैं।
- **कार्यान्वयन विवरण**: `getArgs` एक IO फंक्शन है, जिसका मतलब है कि यह मुख्य IO एक्शन में काम करेगा और सिस्टम के बाहरी वール्ड के साथ बातचीत करने में सक्षम है।

## और भी देखें

- [Haskell में कमांड लाइन आर्गुमेंट्स कैसे इस्तेमाल करें](https://www.haskell.org/tutorial/io.html)
- [ऑप्शन पार्सिंग: optparse-applicative](https://hackage.haskell.org/package/optparse-applicative)
- [कमांड लाइन इंटरफ़ेस डिजाइन](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Parsing)