---
title:                "वर्तमान तारीख प्राप्त करना"
date:                  2024-01-20T15:15:14.285794-07:00
simple_title:         "वर्तमान तारीख प्राप्त करना"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)

करंट डेट पाना यानी आज की तारीख का पता लगाना है। प्रोग्रामर्स लॉग्स, टाइमस्टैम्प्स, और रोज़मर्रा की फंक्शनालिटी जैसे चीजों के लिए तारीख का इस्तेमाल करते हैं। 

## कैसे करें? (How to:)

```Haskell
-- आपको 'time' लाइब्रेरी को import करने की ज़रूरत होगी
import Data.Time

-- मुख्य फंक्शन जिससे कार्यक्रम को चलाएंगे
main :: IO ()
main = do
    currentDate <- getCurrentTime
    print $ utctDay currentDate

-- इसे चलाने के बाद, आपको समान आउटपुट दिखाई देगा:
-- "YYYY-MM-DD"
```

## गहराई की जानकारी (Deep Dive)

'getCurrentTime' फंक्शन `time` लाइब्रेरी का एक हिस्सा है और ये Coordinated Universal Time (UTC) में मौजूदा समय देता है। इसे Haskell में जोड़ने का मकसद था प्रोग्रामर्स को सिस्टम के करंट टाइम के साथ सीधा इंटरैक्शन करने का साधन उपलब्ध कराना। वैकल्पिक तरीकों में सिस्टम का 'calendar time' हो सकता है, लेकिन UTC व्यापक रूप से सिस्टम के टाइम जोन को नज़रअंदाज़ करते हुए समय साझा करने का एक मानक है। इंप्लीमेंटेशन में क्लॉक टाइम से समय लेकर, उसे UTC में बदलने का काम होता है, जिससे हमें मौजूदा समय मिलता है।

## देखें भी (See Also)

- Haskell Documentation for `time` library: https://hackage.haskell.org/package/time
- System.Time Module (obsolete): https://hackage.haskell.org/package/old-time-1.1.0.3/docs/System-Time.html
- Coordinated Universal Time (UTC): https://www.timeanddate.com/time/aboututc.html
- Understanding Time Zones in Haskell: https://wiki.haskell.org/Understanding_time_zones
