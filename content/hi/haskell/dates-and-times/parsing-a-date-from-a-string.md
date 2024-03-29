---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:23.345279-07:00
description: "\u0939\u093E\u0938\u094D\u0915\u0947\u0932 \u092E\u0947\u0902 \u090F\
  \u0915 \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0938\u0947 \u0924\u093E\
  \u0930\u0940\u0916 \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\u0928\u093E \u0924\
  \u093E\u0930\u0940\u0916\u094B\u0902 \u0915\u0947 \u092E\u094C\u0916\u093F\u0915\
  \ \u092A\u094D\u0930\u0924\u093F\u0928\u093F\u0927\u093F\u0924\u094D\u0935 \u0915\
  \u094B \u090F\u0915 \u0938\u0902\u0930\u091A\u093F\u0924 \u092A\u094D\u0930\u093E\
  \u0930\u0942\u092A \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\u0947 \u0915\u0940\
  \ \u092A\u094D\u0930\u0915\u094D\u0930\u093F\u092F\u093E \u0939\u094B\u0924\u0940\
  \ \u0939\u0948 \u091C\u093F\u0938\u0947 \u0915\u093E\u0930\u094D\u092F\u0915\u094D\
  \u0930\u092E \u092E\u0947\u0902 \u092C\u0926\u0932\u093E\u0935\u2026"
lastmod: '2024-03-13T22:44:52.421154-06:00'
model: gpt-4-0125-preview
summary: "\u0939\u093E\u0938\u094D\u0915\u0947\u0932 \u092E\u0947\u0902 \u090F\u0915\
  \ \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0938\u0947 \u0924\u093E\u0930\
  \u0940\u0916 \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\u0928\u093E \u0924\u093E\
  \u0930\u0940\u0916\u094B\u0902 \u0915\u0947 \u092E\u094C\u0916\u093F\u0915 \u092A\
  \u094D\u0930\u0924\u093F\u0928\u093F\u0927\u093F\u0924\u094D\u0935 \u0915\u094B\
  \ \u090F\u0915 \u0938\u0902\u0930\u091A\u093F\u0924 \u092A\u094D\u0930\u093E\u0930\
  \u0942\u092A \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\u0947 \u0915\u0940 \u092A\
  \u094D\u0930\u0915\u094D\u0930\u093F\u092F\u093E \u0939\u094B\u0924\u0940 \u0939\
  \u0948 \u091C\u093F\u0938\u0947 \u0915\u093E\u0930\u094D\u092F\u0915\u094D\u0930\
  \u092E \u092E\u0947\u0902 \u092C\u0926\u0932\u093E\u0935\u2026"
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0938\u0947 \u0924\u093E\
  \u0930\u0940\u0916 \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\u0928\u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?

हास्केल में एक स्ट्रिंग से तारीख पार्स करना तारीखों के मौखिक प्रतिनिधित्व को एक संरचित प्रारूप में बदलने की प्रक्रिया होती है जिसे कार्यक्रम में बदलाव करना संभव होता है। यह प्रक्रिया कैलेंडरिकल डेटा से निपटने वाले अनुप्रयोगों के लिए मौलिक होती है, जिससे कार्यकाल की गणना, शेड्यूलिंग और डेटा सत्यापन जैसे कार्यों को सक्षम बनाया जाता है।

## कैसे:

आउट ऑफ द बॉक्स, हास्केल तारीखों को पार्स करने के लिए मूल उपकरण प्रदान करता है, लेकिन `time` जैसे पुस्तकालयों का उपयोग मूल कार्यक्षमता के लिए और `date-parse` या `time-parse` जैसे पुस्तकालयों का उपयोग अधिक लचीले पार्सिंग के लिए कार्य को काफी सरल बना सकता है।

पहले, सुनिश्चित करें कि आपके पास `time` पुस्तकालय उपलब्ध है; यह अक्सर GHC के साथ शामिल होता है, लेकिन अगर आपको इसे निर्भरता के रूप में निर्दिष्ट करने की आवश्यकता हो तो, अपनी परियोजना की कैबल फाइल में `time` जोड़ें या `cabal install time` का उपयोग करके इसे मैन्युअली इंस्टॉल करें।

```haskell
import Data.Time.Format
import Data.Time.Clock
import System.Locale (defaultTimeLocale)

-- समय पुस्तकालय का उपयोग करके एक मानक प्रारूप में तारीख को पार्स करना
parseBasicDate :: String -> Maybe UTCTime
parseBasicDate = parseTimeM True defaultTimeLocale "%Y-%m-%d" 
```

उदाहरण का उपयोग और आउटपुट:

```haskell
main :: IO ()
main = print $ parseBasicDate "2023-04-01"

-- आउटपुट: Just 2023-03-31 22:00:00 UTC
```

अधिक जटिल परिस्थितियों के लिए, जहां आपको कई प्रारूपों या स्थानों को संभालने की आवश्यकता है, तृतीय-पक्ष पुस्तकालय जैसे कि `date-parse` अधिक सुविधाजनक हो सकते हैं:

मान लें कि आपने अपनी निर्भरताओं में `date-parse` जोड़ा है और इसे इंस्टॉल किया है, यहाँ आप इसका उपयोग कैसे कर सकते हैं:

```haskell
import Data.Time.Calendar
import Text.Date.Parse (parseDate)

-- date-parse पुस्तकालय के साथ एक तारीख स्ट्रिंग को पार्स करना कई प्रारूपों का समर्थन करता है
parseFlexibleDate :: String -> Maybe Day
parseFlexibleDate = parseDate
```

`date-parse` के साथ उदाहरण का उपयोग:

```haskell
main :: IO ()
main = print $ parseFlexibleDate "April 1, 2023"

-- आउटपुट: Just 2023-04-01
```

प्रत्येक उदाहरण हास्केल में एक स्ट्रिंग को उपयोगी तारीख ऑब्जेक्ट में बदलने की मौलिक प्रक्रिया को प्रदर्शित करता है। `time` पुस्तकालय के निर्मित कार्यों का उपयोग करने और `date-parse` जैसे तृतीय-पक्ष समाधान के लिए चयन आपके अनुप्रयोग की विशिष्ट आवश्यकताओं पर निर्भर करता है, जैसे कि आपको संभालने की आवश्यकता वाले इनपुट प्रारूपों की श्रेणी।
