---
title:                "प्रत्येक प्रक्रिया से सेक्रेट पाएजनरेटिंग: मामूली गणित"
html_title:           "Haskell: प्रत्येक प्रक्रिया से सेक्रेट पाएजनरेटिंग: मामूली गणित"
simple_title:         "प्रत्येक प्रक्रिया से सेक्रेट पाएजनरेटिंग: मामूली गणित"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## क्या है और क्यों?

बहुत से हस्केल प्रोग्रामर्स अपनी प्रोग्राम में रैंडम संख्याओं का उपयोग करते है। रैंडम संख्याएं हस्केल में तय नहीं होती हैं और हर बार की तरह प्रोग्राम चलाने पर नए संख्याएं बनाई जाती हैं। रैंडम संख्याओं का उपयोग डेटा ऐनालिसिस और सम्पादन, सिक्युरिटी और अन्य एप्लिकेशन्स में किया जाता है।

## कैसे करें:

```Haskell
import System.Random

-- सामान्य तरीके से से एक रैंडम संख्या बनाने के लिए:
randNumber <- randomIO :: IO Int
print randNumber
-- output: कोई रैंडम संख्या जैसे 872312 दिखाएगा

-- एक निश्चित रेंज के बीच रैंडम संख्या बनाएँ:
randNumInRange <- randomRIO (1, 10) :: IO Int
print randNumInRange
-- output: 1 से 10 के बीच से कोई रैंडम संख्या दिखाएगी
```

## गहराई में जाएँ:

रन्डम संख्याओं का आविष्कार गणितीय संख्यात्मक विज्ञान के विस्तृत क्षेत्र में सबसे अग्रणी अवधारणाओं में से एक है। हस्केल में, `System.Random` मॉड्यूल का उपयोग संख्याओं के ठोस मापदंड और अन्य संयुक्त मापदंडों के लिए किया जाता है। वैकल्पिक तरीके से, "mwc-random" जैसे बाहरी पुस्तकालयों का भी उपयोग किया जा सकता है। रैंडम संख्याओं को उत्पन्न करने का प्रमुख तरीका "नेतुनी धन रंगीनीकरण" है, जो संख्यात्मक विज्ञान में अपनी जगह अर्जित करता है।

## और देखें:

- [Haskell Wiki on Random Numbers](https://wiki.haskell.org/Random_numbers)
- [System.Random Module](https://hackage.haskell.org/package/random-1.2.0/docs/System-Random.html)
- [mwc-random Library](https://hackage.haskell.org/package/mwc-random)