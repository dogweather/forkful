---
title:                "रेगुलर एक्सप्रेशन्स का उपयोग करना"
aliases:
- /hi/haskell/using-regular-expressions/
date:                  2024-02-03T19:17:53.863042-07:00
model:                 gpt-4-0125-preview
simple_title:         "रेगुलर एक्सप्रेशन्स का उपयोग करना"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
प्रोग्रामिंग में नियमित अभिव्यक्तियाँ ऐसे चरित्रों के क्रम होते हैं जो एक खोज पैटर्न को परिभाषित करते हैं, आमतौर पर स्ट्रिंग खोजने और संशोधन के लिए इस्तेमाल किया जाता है। Haskell प्रोग्रामर सरल स्ट्रिंग मिलान से लेकर जटिल पाठ प्रोसेसिंग तक के कार्यों के लिए नियमित अभिव्यक्तियों का उपयोग करते हैं, टेक्स्ट डेटा के साथ निपटने में उनकी दक्षता और बहुमुखी प्रतिभा का लाभ उठाते हैं।

## कैसे:
Haskell में, regex कार्यक्षमताएँ मानक पुस्तकालय का हिस्सा नहीं होती हैं, जिससे `regex-base` जैसे तृतीय-पक्ष पैकेजों का उपयोग और `regex-posix` (POSIX regex समर्थन के लिए), `regex-pcre` (Perl-संगत regex के लिए), आदि जैसे संगत बैकएंड की आवश्यकता होती है। यहाँ बताया गया है कि आप नियमित अभिव्यक्तियों के साथ काम करने के लिए इन पैकेजों का उपयोग कैसे कर सकते हैं।

सबसे पहले, सुनिश्चित करें कि `regex-posix` या `regex-pcre` को आपकी प्रोजेक्ट की `.cabal` फ़ाइल में जोड़कर या सीधे cabal के माध्यम से इंस्टॉल करके पैकेज इंस्टॉल हों:

```bash
cabal install regex-posix
```
या
```bash
cabal install regex-pcre
```

### `regex-posix` का उपयोग करना:

```haskell
import Text.Regex.Posix ((=~))

-- चेक करें कि क्या एक स्ट्रिंग पैटर्न से मेल खाता है
isMatch :: String -> String -> Bool
isMatch text pattern = text =~ pattern :: Bool

-- पहला मेल ढूँढें
findFirst :: String -> String -> String
findFirst text pattern = text =~ pattern :: String

main :: IO ()
main = do
    print $ isMatch "hello world" "wo"
    -- आउटपुट: True
    print $ findFirst "good morning, good night" "good"
    -- आउटपुट: "good"
```

### `regex-pcre` का उपयोग करना:

```haskell
import Text.Regex.PCRE ((=~))

-- सभी मिलानों को खोजें
findAll :: String -> String -> [String]
findAll text pattern = text =~ pattern :: [String]

main :: IO ()
main = do
    print $ findAll "test1 test2 test3" "\\btest[0-9]\\b"
    -- आउटपुट: ["test1","test2","test3"]
```

प्रत्येक लाइब्रेरी की अपनी विशेषताएँ होती हैं, लेकिन मैच की जाँच करने या उपशिरों को निकालने के लिए `=~` का उपयोग करने की सामान्य पद्धति लगातार बनी रहती है। `regex-posix` या `regex-pcre` के बीच चयन करना आमतौर पर आपकी प्रोजेक्ट की जरूरतों और विशेष regex क्षमताओं के आधार पर होता है।
