---
title:                "यादृच्छिक संख्याओं का निर्माण"
html_title:           "Clojure: यादृच्छिक संख्याओं का निर्माण"
simple_title:         "यादृच्छिक संख्याओं का निर्माण"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## व्हाट एंड वाय?

यादृच्छिक संख्या उत्पन्न करना एक प्रक्रिया है जिसमें कंप्यूटर कोई अनुपम संख्या उत्पन्न करता है, जो पहले से योजना बनाकर नहीं जानी जा सकती है। यह प्रोग्रामर्स के लिए महत्वपूर्ण होता है जब किसी गणना में या कोई ऐप्लिकेशन टेस्टिंग में एक रैंडम प्रविष्टि की आवश्यकता हो‌ती है।

## हाउ टू:

```Haskell
import System.Random
main :: IO ()
main = do
    number <- randomIO :: IO Int
    print(number)
```
जब आप इस कोड को चलाते हैं, तो यह हर बार एक अलग यादृच्छिक पूर्णांक उत्पन्न करता है।

## डीप डाइव:

1. इतिहासिक संदर्भ: यादृच्छिक संख्याओं के उत्पादन का विचार पहले परंपरागत कंप्यूटर साइंस में आया था, जिसका उद्देश्य क्रिप्टोग्राफी और सिमुलेशन में अविश्वसनीयता जोड़ना था।

2. विकल्प: Haskell में System.Random बाहरी पैकेज है जो यादृच्छिकता का समर्थन करता है। एक और विकल्प है `mwc-random` पैकेज जो हाई परफॉर्मेंस प्रदान करता है।

3. कार्यान्वयन विवरण: Haskell का `randomIO` फ़ंक्शन एक यादृच्छिक संख्या उत्पन्न करता है जिसे IO Monad में उठाया जाता है, यानी कि इसे कार्यान्वित करने के लिए आउटपुट निर्भर करना होता है।

## देखें भी:

2. [Haskell Docs: System.Random](https://hackage.haskell.org/package/random-1.1/docs/System-Random.html)
3. [Introduction to Randomness and Random Numbers by Dr Mads Haahr](https://www.random.org/randomness/)
4. [Haskell Stack Overflow: How do you generate a random int value in Haskell?](https://stackoverflow.com/questions/30740366/list-with-random-numbers-in-haskell)
5. [mwc-random package](http://hackage.haskell.org/package/mwc-random)