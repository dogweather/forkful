---
title:                "एक तारीख को स्ट्रिंग में परिवर्तित करना"
html_title:           "Java: एक तारीख को स्ट्रिंग में परिवर्तित करना"
simple_title:         "एक तारीख को स्ट्रिंग में परिवर्तित करना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों? 

डेट को एक स्ट्रिंग में बदलना मतलब किसी कैलेंडर दिनांक को टेक्स्ट के रूप में प्रस्तुत करना। प्रोग्रामर इसे करते हैं क्योंकि यह उन्हें डेटा को साढ़े और पठनीय रूप में प्रस्तुत करने की अनुमति देता है। 

## कैसे करें:

आंटर के कोड स्निपेट का उपयोग करके, हम एक दिनांक को किसी स्ट्रिंग में बदल सकते हैं:

```Haskell
import Data.Time
import System.Locale

main = do
now <- getZonedTime
let formatter = formatTime defaultTimeLocale "%F" now
print formatter
```

जब आप इस कोड को चलाते हैं, आपको निम्नलिखित आउटपुट मिलेगा (आइये, आज की तारीख है "YYYY-MM-DD"):

```Haskell
"YYYY-MM-DD"
```

## गहराई से देखें:

इस विधि का उपयोग करने के पीछे इतिहास तक पहुंचने के लिए, यह ISO 8601 दिनांक और समय विन्यास मानदंड का पालन करता है जो 1988 में स्थापित किया गया था। 

इसके विकल्पों में अन्य दिनांक-स्ट्रिंग प्रारूप हैं जिन्हें आप उपयोग कर सकते हैं, जैसे कि "%D" (MM/DD/YY) या "%x" (स्थानीय तारीख का प्रारूप). आपके उपयोग के लिए कौन सा प्रारूप सबसे उपयुक्त है, यह आपकी आवश्यकताओं पर निर्भर करेगा। 

विवरण के लिए, `formatTime` गुण अर्जुमेंट को अपनाता है और छावनी के रुप में मान वापस करता है, जिसे हम फिर प्रिंट करते हैं। 

## और भी देखें: 

- ISO 8601 मानदंड: https://www.iso.org/iso-8601-date-and-time-format.html
- Haskell द्वारा इस्तेमाल किए जाने वाले सभी प्रारूप धोरियों: https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html
- Haskell 'time' पैकेज: https://hackage.haskell.org/package/time