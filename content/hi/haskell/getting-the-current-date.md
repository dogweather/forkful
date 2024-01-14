---
title:    "Haskell: तारीख को प्राप्त करना"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## क्यों

क्या आप भी अपनी Haskell प्रोग्राम में मौजूदा तारीख प्राप्त करने के लिए उत्सुक हैं? यह आम है कि हम प्रोग्रामिंग में अपनी रुचि के अनुसार हर समय नए नए काम करना चाहते हैं और तारीख हमारे काम को आसान बनाने में मदद कर सकती है। इस ब्लॉग पोस्ट के माध्यम से हम देखेंगे कि हम कैसे Haskell में वर्तमान तारीख प्राप्त कर सकते हैं। 

## कैसे करें

हम Haskell के `getCurrentTime` फंक्शन का उपयोग करके स्थानीय समय को जान सकते हैं। इसे एक उदाहरण के साथ समझते हैं:

```Haskell
import Data.Time.Clock

main = do
  now <- getCurrentTime
  print now
```

यह कोड रन करने से हमें आज की वर्तमान समय दिखाई देगा, जैसे `2021-11-08 12:15:00.925468 UTC`। हम यदि चाहें तो समय को प्रारूपित कर सकते हैं जैसे कि `YYYY/MM/DD` या `HH:MM`। 

```Haskell
import Data.Time.Clock
import Data.Time.Format

main = do
  now <- getCurrentTime
  let formattedDate = formatTime defaultTimeLocale "%Y/%m/%d" now
  print formattedDate -- उत्पाद: 2021/11/08
  let formattedTime = formatTime defaultTimeLocale "%H:%M" now
  print formattedTime -- उत्पाद: 12:15
```

## गहराई में जायें

जब हम Haskell में यह कैसे करें, की स्पष्ट रूप से जानकारी हासिल कर लेते हैं तो हमें अपनी प्रोग्राम में समय और दिनांक से सम्बंधित अन्य फंक्शन को भी आसानी से उपयोग करने की सुविधा मिलती है। इससे हमें अपने प्रोग्राम को और भी उपयोगी बनाने के लिए भी सक्षम होते हैं। हम इस ब्लॉग पोस्ट में गहराई से इसे समझेंगे। 

## देखें भी 

- [Haskell डेटा टाइम पैकेज](https://hackage.haskell.org/package/time