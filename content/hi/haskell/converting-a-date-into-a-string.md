---
title:    "Haskell: तारीख को स्ट्रिंग में रूपांतरित करना"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## क्यों

क्या आप प्रोग्रामिंग में तारीख को स्ट्रिंग में परिवर्तित करने के बारे में सोच रहे हो? हां, अवश्य! तारीखों को स्ट्रिंग में परिवर्तित करने का मतलब है कि आप उन्हें दूसरों के साथ साझा कर सकते हैं और उन्हें स्टोर कर सकते हैं। यह आपको अपने कोड में तारीखों को बहुत स्पष्ट और सुलभ बनाता है।

## कैसे करें

आइए जानें कि हम Haskell में एक तारीख को स्ट्रिंग में परिवर्तित कैसे कर सकते हैं। सबसे पहले, हम ध्यान देंगे कि तारीख को स्ट्रिंग में परिवर्तित करने के लिए हम कई फंक्शन और लाइब्रेरी उपयोग कर सकते हैं। लेकिन हम यहां एक आसानी से समझने और सार्थक उदाहरण के साथ एक बुनियादी तरीका सीखेंगे।

```Haskell
-- यह कोड टेम्पलेट है, आप अपने अनुसार इसे बदल सकते हैं
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

-- सबसे पहले, हम वर्तमान काल प्राप्त करते हैं
main = do
    now <- getCurrentTime
    let date = dateToString now
    putStrLn $ "स्ट्रिंग में तारीख: " ++ date

-- इस फंक्शन द्वारा हम तारीख को स्ट्रिंग में परिवर्तित करते हैं
-- आप अपनी तारीख के अनुसार इसे बदल सकते हैं
dateToString :: UTCTime -> String
dateToString date = formatTime defaultTimeLocale "%d %B %Y" date

-- यह दूसरा फंक्शन हमें एक अक्षरिक तारीख मिलाता है
-- जो हमें अपनी तारीख के अनुसार फार्मेट करने देता है
dateToSimpleString :: UTCTime -> String
dateToSimpleString date = formatTime defaultTimeLocale "%d-%m-%Y" date
```

आउटपुट:
```
स