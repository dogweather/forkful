---
title:                "कॉम्प्यूटर प्रोग्रामिंग में csv के साथ काम करना"
html_title:           "Haskell: कॉम्प्यूटर प्रोग्रामिंग में csv के साथ काम करना"
simple_title:         "कॉम्प्यूटर प्रोग्रामिंग में csv के साथ काम करना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
CSV काम करना क्या है और क्यों कोडर्स ऐसा करते हैं उसका अवलोकन किया जाएगा। CSV फाइलें टेक्स्ट फाइलें होती हैं जो डेटा को एक दूसरे से अलग करने के लिए उपयोग की जाती हैं। काम करते समय, हमें इस डेटा को एक टेबल की तरह से संरचित करना होता है। 

## कैसे:
यहां हम हेस्कल की मदद से CSV फ़ाइलों का इस्तेमाल करना सीखेंगे। पहले, हम फाइल को ओपन करेंगे। फिर, फाइल से डेटा पढ़ेंगे और उसे एक सूची में डालेंगे। अंत में, हम CSV फ़ाइल में शामिल हैडर और देश कोड को प्रिंट करेंगे।

```Haskell
import System.IO
import Data.List.Split

main = do
  handle <- openFile "countrycodes.csv" ReadMode
  contents <- hGetContents handle
  let listOfCodes = splitOn "\n" contents
  let header = head listOfCodes
  let countryCodes = tail listOfCodes
  putStrLn header
  putStrLn $ head countryCodes
  hClose handle
  
-- Output:
-- Country,Code
-- Afghanistan,AF
```

## डीप डाइव:
CSV का उपयोग स्प्रेडशीट्स और डेटा बेस्ड सोफ्टवेयर में डेटा को संकलित करने के लिए किया जाता है। इसमें और भी कई विकल्प हैं जैसे TSV (Tab Separated Values) या JSON। हेस्कल में CSV फाइलें पिएर डेटा टाइप के रूप में हैं, जो कि उत्पादन स्तर पर इंटरनेशनली एकीकृत चरित्रों को समर्थित करता है। 

## देखें भी:
- [हेस्कल के लिए CSV पैकेज](https://hackage.haskell.org/package/csv)