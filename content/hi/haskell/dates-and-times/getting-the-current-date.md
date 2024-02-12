---
title:                "वर्तमान तारीख प्राप्त करना"
aliases: - /hi/haskell/getting-the-current-date.md
date:                  2024-02-03T19:11:13.376049-07:00
model:                 gpt-4-0125-preview
simple_title:         "वर्तमान तारीख प्राप्त करना"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
Haskell में वर्तमान तारीख प्राप्त करना सिस्टम के वर्तमान समय को प्राप्त करने और इसे पढ़ने योग्य तारीख प्रारूप में बदलने को शामिल करता है। प्रोग्रामर ऐसा तारीख के आधार पर ऑपरेशन करने के लिए करते हैं, जैसे कि लॉगिंग, कार्यों की शेड्यूलिंग, या एप्लिकेशन में घटनाओं को टाइमस्टैम्पिंग करना।

## कैसे करें:
Haskell की स्टैंडर्ड लाइब्रेरी, `base`, `Data.Time` मॉड्यूल प्रदान करती है जो तारीखों और समय के साथ काम करने की कार्यक्षमता प्रदान करती है। यहाँ वर्तमान तारीख प्राप्त करने के लिए इसका उपयोग कैसे करें:

```haskell
import Data.Time (getCurrentTime, utctDay)

main :: IO ()
main = do
    now <- getCurrentTime
    let today = utctDay now
    print today
```

नमूना आउटपुट:
```
2023-04-12
```

तारीख को स्वरूपित करने या विभिन्न समय क्षेत्रों के साथ काम करने जैसी अधिक लचीलापन के लिए, `time` लाइब्रेरी अमूल्य है। यहाँ वर्तमान तारीख को स्वरूपित करने का एक तरीका कैसे होगा:

```haskell
import Data.Time

main :: IO ()
main = do
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    let zoneNow = utcToLocalTime timezone now
    putStrLn $ formatTime defaultTimeLocale "%Y-%m-%d" zoneNow
```

यह `YYYY-MM-DD` प्रारूप में, स्थानीय समय क्षेत्र के अनुकूलित, वर्तमान तारीख को प्रिंट करता है।

अतिरिक्त रूप से, तीसरे पक्ष की लाइब्रेरी सहायता के लिए, तारीख और समय की व्यापक परिवर्तन क्षमताओं के लिए `time` की अत्यधिक सिफारिश की जाती है और अक्सर Haskell समुदाय के भीतर इसका उपयोग किया जाता है। उपरोक्त उदाहरणों में इस लाइब्रेरी का इस्तेमाल होता है।

अगर आपको तारीखों और समयों के साथ स्ट्रिंग्स से पार्सिंग या अंकगणितीय ऑपरेशन जैसे अधिक व्यापक तारीख परिवर्तनों की आवश्यकता है, तो `Data.Time` के भीतर अतिरिक्त फ़ंक्शन्स का अन्वेषण लाभकारी होगा।
