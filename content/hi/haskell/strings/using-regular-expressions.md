---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:53.863042-07:00
description: "\u0915\u0948\u0938\u0947: Haskell \u092E\u0947\u0902, regex \u0915\u093E\
  \u0930\u094D\u092F\u0915\u094D\u0937\u092E\u0924\u093E\u090F\u0901 \u092E\u093E\u0928\
  \u0915 \u092A\u0941\u0938\u094D\u0924\u0915\u093E\u0932\u092F \u0915\u093E \u0939\
  \u093F\u0938\u094D\u0938\u093E \u0928\u0939\u0940\u0902 \u0939\u094B\u0924\u0940\
  \ \u0939\u0948\u0902, \u091C\u093F\u0938\u0938\u0947 `regex-base` \u091C\u0948\u0938\
  \u0947 \u0924\u0943\u0924\u0940\u092F-\u092A\u0915\u094D\u0937 \u092A\u0948\u0915\
  \u0947\u091C\u094B\u0902 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0914\u0930\
  \ `regex-posix` (POSIX\u2026"
lastmod: '2024-03-13T22:44:52.386966-06:00'
model: gpt-4-0125-preview
summary: "Haskell \u092E\u0947\u0902, regex \u0915\u093E\u0930\u094D\u092F\u0915\u094D\
  \u0937\u092E\u0924\u093E\u090F\u0901 \u092E\u093E\u0928\u0915 \u092A\u0941\u0938\
  \u094D\u0924\u0915\u093E\u0932\u092F \u0915\u093E \u0939\u093F\u0938\u094D\u0938\
  \u093E \u0928\u0939\u0940\u0902 \u0939\u094B\u0924\u0940 \u0939\u0948\u0902, \u091C\
  \u093F\u0938\u0938\u0947 `regex-base` \u091C\u0948\u0938\u0947 \u0924\u0943\u0924\
  \u0940\u092F-\u092A\u0915\u094D\u0937 \u092A\u0948\u0915\u0947\u091C\u094B\u0902\
  \ \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0914\u0930 `regex-posix` (POSIX\
  \ regex \u0938\u092E\u0930\u094D\u0925\u0928 \u0915\u0947 \u0932\u093F\u090F), `regex-pcre`\
  \ (Perl-\u0938\u0902\u0917\u0924 regex \u0915\u0947 \u0932\u093F\u090F), \u0906\u0926\
  \u093F \u091C\u0948\u0938\u0947 \u0938\u0902\u0917\u0924 \u092C\u0948\u0915\u090F\
  \u0902\u0921 \u0915\u0940 \u0906\u0935\u0936\u094D\u092F\u0915\u0924\u093E \u0939\
  \u094B\u0924\u0940 \u0939\u0948\u0964 \u092F\u0939\u093E\u0901 \u092C\u0924\u093E\
  \u092F\u093E \u0917\u092F\u093E \u0939\u0948 \u0915\u093F \u0906\u092A \u0928\u093F\
  \u092F\u092E\u093F\u0924 \u0905\u092D\u093F\u0935\u094D\u092F\u0915\u094D\u0924\u093F\
  \u092F\u094B\u0902 \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\
  \u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0907\u0928 \u092A\u0948\u0915\u0947\
  \u091C\u094B\u0902 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0948\u0938\
  \u0947 \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964\n\n\u0938\u092C\
  \u0938\u0947 \u092A\u0939\u0932\u0947, \u0938\u0941\u0928\u093F\u0936\u094D\u091A\
  \u093F\u0924 \u0915\u0930\u0947\u0902 \u0915\u093F `regex-posix` \u092F\u093E `regex-pcre`\
  \ \u0915\u094B \u0906\u092A\u0915\u0940 \u092A\u094D\u0930\u094B\u091C\u0947\u0915\
  \u094D\u091F \u0915\u0940 `.cabal` \u092B\u093C\u093E\u0907\u0932 \u092E\u0947\u0902\
  \ \u091C\u094B\u0921\u093C\u0915\u0930 \u092F\u093E \u0938\u0940\u0927\u0947 cabal\
  \ \u0915\u0947 \u092E\u093E\u0927\u094D\u092F\u092E \u0938\u0947 \u0907\u0902\u0938\
  \u094D\u091F\u0949\u0932 \u0915\u0930\u0915\u0947 \u092A\u0948\u0915\u0947\u091C\
  \ \u0907\u0902\u0938\u094D\u091F\u0949\u0932 \u0939\u094B\u0902."
title: "\u0930\u0947\u0917\u0941\u0932\u0930 \u090F\u0915\u094D\u0938\u092A\u094D\u0930\
  \u0947\u0936\u0928\u094D\u0938 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\
  \u0930\u0928\u093E"
weight: 11
---

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
