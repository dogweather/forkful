---
title:                "एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
html_title:           "Kotlin: एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
simple_title:         "एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

एक string को lower case में बदलने का मतलब होता है सभी अक्षरों को छोटे अक्षरों में बदल देना। प्रोग्रामर इसे यादृच्छिकता और त्रुटियों को रोकने के लिए करते हैं, जैसे कि जब प्रयोगकर्ताओं द्वारा इंपुट दिया गया डाटा को मिलाने या तुलना करने की आवश्यकता होती है।

## कैसे करें:

आप `Data.Char` module का upyog करके `toLower` function का upyog kar sakte hain. 
```Haskell
import Data.Char (toLower)

convertToLower :: String -> String
convertToLower = map toLower

main = print $ convertToLower "HELLO WORLD"
```
Output:
```Haskell
"hello world"
```

## गहराई में:
* ऐतिहासिक प्रसंग: Haskell में strings को lower case में बदलने का समर्थन भाषा के अविभाज्य हिस्से के रूप में शामिल है।
* विकल्प: Haskell में कई अन्य विधियां भी हैं जो यह कार्य कर सकती हैं, जैसे कि: `Text` टाइप का upyog करके जिसे `Data.Text` module में परिभाषित किया गया है।
* कार्यान्वयन विवरण: `toLower` function `Char` को `Char` {'a' से 'z'} में मापता है। यदि यह पहले से ही lower case है, तो इसे बदला नहीं जाता है।

## और भी देखें:

* [Haskell String Type](https://www.haskell.org/tutorial/strings.html)
* [Haskell Data.Char Module](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Char.html)
* [Haskell Data.Text Module](https://hackage.haskell.org/package/text-1.2.4.1/docs/Data-Text.html)